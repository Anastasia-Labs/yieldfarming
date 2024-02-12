module Main (main) where

import Spec.YieldFarmingSpec (unitTest, yieldFarmingProperties)
import Test.Tasty (defaultMain, testGroup)

main :: IO ()
main = do
  defaultMain $
    testGroup
      "Unit Test Group"
      [ unitTest
      , yieldFarmingProperties
      ]
