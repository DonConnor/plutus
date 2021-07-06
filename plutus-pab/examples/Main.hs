module Main
    ( main
    ) where

import           ContractExample (handleContractExample)
import           Plutus.PAB.Run  (runWith)

main :: IO ()
main = do
    runWith handleContractExample
