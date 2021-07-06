{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

{-

'beam' handler for the 'ContractDefinitionStore' effect

-}
module Plutus.PAB.Db.Beam.ContractDefinitionStore
  -- (handleContractDefinitionStore)
  where

-- import           Control.Monad.Freer                 (Eff, Member, type (~>))
-- import qualified Data.Text                           as Text
-- import           Database.Beam                       (all_, select)
-- import           Plutus.PAB.Effects.Contract         (ContractDefinitionStore (..))
-- import           Plutus.PAB.Effects.Contract.Builtin (Builtin)
-- import           Plutus.PAB.Effects.DbStore          hiding (contractPath)

-- -- mkRow :: ContractExe -> Contract
-- -- mkRow (ContractExe {contractPath}) = Contract (Text.pack contractPath)
-- mkRow :: (Show a) => a -> Contract
-- mkRow = Contract . Text.pack . show -- TODO: is it correct to use 'show'

-- -- fromRow :: Contract -> ContractExe
-- -- fromRow (Contract {_contractPath})  = ContractExe (Text.unpack _contractPath)
-- fromRow :: forall a. Read a => Contract -> a
-- fromRow Contract {_contractId}  = read $ Text.unpack _contractId -- TODO: is it correct to use 'read'

-- -- | Run the 'ContractDefinitionStore' effect in the context of the 'DbStore'.
-- handleContractDefinitionStore ::
--   forall a effs.
--   ( Member DbStoreEffect effs
--   , Show a
--   , Read a
--   )
--   => ContractDefinitionStore (Builtin a)
--   ~> Eff effs
-- handleContractDefinitionStore = \case
--   AddDefinition t -> addRow (_contracts db) (mkRow t)
--   GetDefinitions  -> map fromRow <$> selectList (select (all_ (_contracts db)))
