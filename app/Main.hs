{-# OPTIONS_GHC -Wno-unused-imports #-}

module Main (main) where

import Utils (writePlutusScript)
import YieldFarming qualified

import System.Directory (createDirectoryIfMissing, doesDirectoryExist)

main :: IO ()
main = do
  writePlutusScript "Yield Farming Validator" "./compiled/yieldFarming.json" YieldFarming.pvalidateYieldFarmW
