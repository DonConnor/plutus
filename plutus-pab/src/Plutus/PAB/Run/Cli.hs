{-# LANGUAGE ApplicativeDo         #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DerivingVia           #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StrictData            #-}
{-# LANGUAGE TypeApplications      #-}

module Plutus.PAB.Run.Cli (ConfigCommandArgs(..), runConfigCommand, runNoConfigCommand) where

-----------------------------------------------------------------------------------------------------------------------
-- Command interpretation
-----------------------------------------------------------------------------------------------------------------------

import           Cardano.BM.Configuration            (Configuration)
import qualified Cardano.BM.Configuration.Model      as CM
import           Cardano.BM.Data.Trace               (Trace)
import qualified Cardano.ChainIndex.Server           as ChainIndex
import qualified Cardano.Metadata.Server             as Metadata
import qualified Cardano.Node.Server                 as NodeServer
import           Cardano.Node.Types                  (MockServerConfig (..))
import qualified Cardano.Wallet.Server               as WalletServer
import           Cardano.Wallet.Types
import           Control.Concurrent                  (takeMVar)
import           Control.Concurrent.Async            (Async, async, waitAny)
import           Control.Concurrent.Availability     (Availability, starting)
import           Control.Monad                       (void)
import           Control.Monad.Freer                 (Eff, Member, interpret)
import           Control.Monad.Freer.Delay           (DelayEffect, delayThread)
import           Control.Monad.Freer.Extras.Log      (logInfo)
import           Control.Monad.IO.Class              (liftIO)
import           Data.Aeson                          (FromJSON, ToJSON)
import           Data.Foldable                       (traverse_)
import qualified Data.Map                            as Map
import           Data.Proxy                          (Proxy (..))
import qualified Data.Set                            as Set
import qualified Data.Text                           as Text
import           Data.Time.Units                     (Second)
import           Plutus.Contract.Resumable           (responses)
import           Plutus.Contract.State               (State (..))
import qualified Plutus.Contract.State               as State
import qualified Plutus.PAB.App                      as App
import qualified Plutus.PAB.Core                     as Core
import qualified Plutus.PAB.Db.Beam                  as Beam
import qualified Plutus.PAB.Effects.Contract         as Contract
import           Plutus.PAB.Effects.Contract.Builtin (Builtin, BuiltinHandler)
import qualified Plutus.PAB.Monitoring.Monitoring    as LM
import           Plutus.PAB.Run.Command
import qualified Plutus.PAB.Run.PSGenerator          as PSGenerator
import           Plutus.PAB.Types                    (Config (..), DbConfig (..), chainIndexConfig,
                                                      metadataServerConfig, nodeServerConfig, walletServerConfig)
import qualified Plutus.PAB.Webserver.Server         as PABServer
import           Plutus.PAB.Webserver.Types          (ContractActivationArgs (..))
import qualified Servant

runNoConfigCommand ::
    Trace IO (LM.AppMsg (Builtin a))  -- ^ PAB Tracer logging instance
    -> NoConfigCommand
    -> IO ()
runNoConfigCommand trace = \case

    -- Run database migration
    Migrate{dbPath} ->
        let conf = DbConfig{dbConfigPoolSize=10, dbConfigFile=Text.pack dbPath} in
        App.migrate (LM.convertLog LM.PABMsg trace) conf

    -- Generate PureScript bridge code
    PSGenerator {outputDir} -> PSGenerator.generate outputDir

    -- Get default logging configuration
    WriteDefaultConfig{outputFile} -> LM.defaultConfig >>= flip CM.exportConfiguration outputFile

data ConfigCommandArgs a =
    ConfigCommandArgs
        { ccaTrace          :: Trace IO (LM.AppMsg (Builtin a))  -- ^ PAB Tracer logging instance
        , ccaLoggingConfig  :: Configuration -- ^ Monitoring configuration
        , ccaPABConfig      :: Config        -- ^ PAB Configuration
        , ccaAvailability   :: Availability  -- ^ Token for signaling service availability
        , ccaStorageBackend :: App.StorageBackend -- ^ Wheter to use the beam-sqlite or in-memory backend
        }

-- | Interpret a 'Command' in 'Eff' using the provided tracer and configurations
--
runConfigCommand :: forall a.
    ( Ord a
    , ToJSON a
    , FromJSON a
    , Show a
    -- , Servant.MimeUnrender Servant.JSON (Contract.ContractDef (Builtin a))
    , Servant.MimeUnrender Servant.JSON a
    )
    => BuiltinHandler a -- (ContractEffect (Builtin a) ~> Eff effs)
    -> ConfigCommandArgs a
    -> ConfigCommand
    -> IO ()

-- Run mock wallet service
runConfigCommand _ ConfigCommandArgs{ccaTrace, ccaPABConfig = Config {nodeServerConfig, chainIndexConfig, walletServerConfig},ccaAvailability} MockWallet =
    liftIO $ WalletServer.main
        (toWalletLog ccaTrace)
        walletServerConfig
        (mscSocketPath nodeServerConfig)
        (mscSlotConfig nodeServerConfig)
        (ChainIndex.ciBaseUrl chainIndexConfig)
        ccaAvailability

-- Run mock node server
runConfigCommand _ ConfigCommandArgs{ccaTrace, ccaPABConfig = Config {nodeServerConfig},ccaAvailability} (MockNode withoutMockServer) =
    liftIO $ NodeServer.main
        (toMockNodeServerLog ccaTrace)
        nodeServerConfig
        withoutMockServer
        ccaAvailability

-- Run mock metadata server
runConfigCommand _ ConfigCommandArgs{ccaTrace, ccaPABConfig = Config {metadataServerConfig}, ccaAvailability} Metadata =
    liftIO $ Metadata.main
        (toMetaDataLog ccaTrace)
        metadataServerConfig
        ccaAvailability

-- Run PAB webserver
runConfigCommand contractHandler ConfigCommandArgs{ccaTrace, ccaPABConfig=config@Config{pabWebserverConfig}, ccaAvailability, ccaStorageBackend} PABWebserver =
        fmap (either (error . show) id)
        $ App.runApp ccaStorageBackend (toPABMsg ccaTrace) contractHandler config
        $ do
            App.AppEnv{App.walletClientEnv} <- Core.askUserEnv @(Builtin a) @(App.AppEnv a)
            (mvar, _) <- PABServer.startServer pabWebserverConfig (Left walletClientEnv) ccaAvailability
            liftIO $ takeMVar mvar

-- Fork a list of commands
runConfigCommand contractHandler c@ConfigCommandArgs{ccaAvailability} (ForkCommands commands) =
    void $ do
        threads <- traverse forkCommand commands
        putStrLn "Started all commands."
        waitAny threads
  where
    forkCommand :: ConfigCommand -> IO (Async ())
    forkCommand subcommand = do
      putStrLn $ "Starting: " <> show subcommand
      asyncId <- async . void . runConfigCommand contractHandler c $ subcommand
      putStrLn $ "Started: " <> show subcommand
      starting ccaAvailability
      pure asyncId

-- Run the chain-index service
runConfigCommand _ ConfigCommandArgs{ccaTrace, ccaPABConfig=Config {nodeServerConfig, chainIndexConfig}, ccaAvailability} ChainIndex =
    ChainIndex.main
        (toChainIndexLog ccaTrace)
        chainIndexConfig
        (mscSocketPath nodeServerConfig)
        (mscSlotConfig nodeServerConfig)
        ccaAvailability

-- Get the state of a contract
runConfigCommand _ ConfigCommandArgs{ccaTrace, ccaPABConfig=Config{dbConfig}} (ContractState contractInstanceId) = do
    connection <- App.dbConnect (LM.convertLog LM.PABMsg ccaTrace) dbConfig
    fmap (either (error . show) id)
        $ Beam.runBeamStoreAction connection (LM.convertLog LM.PABMsg ccaTrace)
        $ interpret (LM.handleLogMsgTrace ccaTrace)
        $ do
            s <- Contract.getState @(Builtin a) contractInstanceId
            let outputState = Contract.serialisableState (Proxy @(Builtin a)) s
            logInfo @(LM.AppMsg (Builtin a)) $ LM.PABMsg $ LM.SCoreMsg $ LM.FoundContract $ Just outputState
            drainLog

-- Get all active contracts
runConfigCommand _ ConfigCommandArgs{ccaTrace, ccaPABConfig=Config{dbConfig}} ReportActiveContracts = do
    connection <- App.dbConnect (LM.convertLog LM.PABMsg ccaTrace) dbConfig
    fmap (either (error . show) id)
        $ Beam.runBeamStoreAction connection (LM.convertLog LM.PABMsg ccaTrace)
        $ interpret (LM.handleLogMsgTrace ccaTrace)
        $ do
            logInfo @(LM.AppMsg (Builtin a)) LM.ActiveContractsMsg
            instancesById <- Contract.getActiveContracts @(Builtin a)
            let idsByDefinition = Map.fromListWith (<>) $ fmap (\(inst, ContractActivationArgs{caID}) -> (caID, Set.singleton inst)) $ Map.toList instancesById
            traverse_ (\(e, s) -> logInfo @(LM.AppMsg (Builtin a)) $ LM.ContractInstances e (Set.toList s)) $ Map.toAscList idsByDefinition
            drainLog

-- Get history of a specific contract
runConfigCommand _ ConfigCommandArgs{ccaTrace, ccaPABConfig=Config{dbConfig}} (ReportContractHistory contractInstanceId) = do
    connection <- App.dbConnect (LM.convertLog LM.PABMsg ccaTrace) dbConfig
    fmap (either (error . show) id)
        $ Beam.runBeamStoreAction connection (LM.convertLog LM.PABMsg ccaTrace)
        $ interpret (LM.handleLogMsgTrace ccaTrace)
        $ do
            logInfo @(LM.AppMsg (Builtin a)) LM.ContractHistoryMsg
            s <- Contract.getState @(Builtin a) contractInstanceId
            let State.ContractResponse{State.newState=State{record}} = Contract.serialisableState (Proxy @(Builtin a)) s
            traverse_ logStep (responses record)
            drainLog
                where
                    logStep response = logInfo @(LM.AppMsg (Builtin a)) $ LM.ContractHistoryItem contractInstanceId (snd <$> response)

toPABMsg :: Trace m (LM.AppMsg (Builtin a)) -> Trace m (LM.PABLogMsg (Builtin a))
toPABMsg = LM.convertLog LM.PABMsg

toChainIndexLog :: Trace m (LM.AppMsg (Builtin a)) -> Trace m LM.ChainIndexServerMsg
toChainIndexLog = LM.convertLog $ LM.PABMsg . LM.SChainIndexServerMsg

toWalletLog :: Trace m (LM.AppMsg (Builtin a)) -> Trace m WalletMsg
toWalletLog = LM.convertLog $ LM.PABMsg . LM.SWalletMsg

toMetaDataLog :: Trace m (LM.AppMsg (Builtin a)) -> Trace m LM.MetadataLogMessage
toMetaDataLog = LM.convertLog $ LM.PABMsg . LM.SMetaDataLogMsg

toMockNodeServerLog :: Trace m (LM.AppMsg (Builtin a)) -> Trace m LM.MockServerLogMsg
toMockNodeServerLog = LM.convertLog $ LM.PABMsg . LM.SMockserverLogMsg

-- | Wait for some time to allow all log messages to be printed to
--   the terminal.
drainLog :: Member DelayEffect effs => Eff effs ()
drainLog = delayThread (1 :: Second)

