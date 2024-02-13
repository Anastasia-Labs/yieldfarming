module Spec.YieldFarmingSpec (unitTest, yieldFarmingProperties) where

import Data.ByteString.Char8 (pack)
import Plutarch.Context (
  UTXO,
  address,
  buildSpending',
  input,
  output,
  signedWith,
  txId,
  withInlineDatum,
  withSpendingOutRefId,
  withValue,
 )
import Plutarch.Prelude
import Plutarch.Test.Precompiled (Expectation (Failure, Success), testEvalCase, tryFromPTerm)
import Plutarch.Test.QuickCheck (fromPFun)
import PlutusLedgerApi.V2 (
  Address (..),
  Credential (..),
  CurrencySymbol (..),
  PubKeyHash (..),
  ScriptContext,
  ScriptHash (..),
  TokenName (..),
  TxId (..),
  Value (..),
  singleton,
 )
import PlutusTx qualified
import PlutusTx.Builtins (BuiltinByteString, toBuiltin)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (Gen, Property, elements, forAll, suchThat, testProperty, vectorOf)
import YieldFarming

currencySymbol :: CurrencySymbol
currencySymbol = "746fa3ba2daded6ab9ccc1e39d3835aa1dfcb9b5a54acc2ebe6b79a4"

tokenName :: TokenName
tokenName = "yield-farming-test"

ownerPKH :: PubKeyHash
ownerPKH = "b1f2f20a8781a3ba967d8c7b5068d21d799e809dcce22f651679d661"

ownerAddress :: Address
ownerAddress = Address (PubKeyCredential ownerPKH) Nothing

scriptHash :: ScriptHash
scriptHash = "b055a795895b15d9af25acb752ac89c78524acfa387acb626c7e1bc8"

scriptAddress :: Address
scriptAddress = Address (ScriptCredential scriptHash) Nothing

datum :: YieldFarmingDatum
datum =
  YieldFarmingDatum
    { owner = ownerAddress
    , lpCS = currencySymbol
    , lpTN = tokenName
    }

terminateRedeemer :: YieldFarmRedeemer
terminateRedeemer = Terminate

terminateScriptContext :: ScriptContext
terminateScriptContext =
  buildSpending' $
    mconcat
      [ signedWith ownerPKH
      ]

badTerminateScriptContext :: ScriptContext
badTerminateScriptContext =
  buildSpending' $
    mconcat []

incorrectPKH :: PubKeyHash
incorrectPKH = "65c4b5e51c3c58c15af080106e8ce05b6efbb475aa5e5c5ca9372a45"

badTerminateScriptContext2 :: ScriptContext
badTerminateScriptContext2 =
  buildSpending' $
    mconcat [signedWith incorrectPKH]

harvestRedeemer :: YieldFarmRedeemer
harvestRedeemer = HarvestRewards {ownIndex = 0}

harvestInTxId :: TxId
harvestInTxId = TxId "2c6dbc95c1e96349c4131a9d19b029362542b31ffd2340ea85dd8f28e271ff6d"

harvestInputUTXO :: UTXO
harvestInputUTXO =
  mconcat
    [ address scriptAddress
    , withValue (singleton "" "" 5_000_000)
    , withInlineDatum datum
    ]

harvestOutputUTXO :: UTXO
harvestOutputUTXO =
  mconcat
    [ address scriptAddress
    , withValue (singleton "" "" 5_000_000)
    , withInlineDatum datum
    ]

changedDatum :: YieldFarmingDatum
changedDatum =
  YieldFarmingDatum
    { owner = ownerAddress
    , lpCS = currencySymbol
    , lpTN = TokenName "changed"
    }

invalidDatumHarvestOutputUTXO :: UTXO
invalidDatumHarvestOutputUTXO =
  mconcat
    [ address scriptAddress
    , withValue (singleton "" "" 5_000_000)
    , withInlineDatum changedDatum
    ]

harvestScriptContext :: ScriptContext
harvestScriptContext =
  buildSpending' $
    mconcat
      [ txId harvestInTxId
      , input harvestInputUTXO
      , output harvestOutputUTXO
      , withSpendingOutRefId harvestInTxId
      , signedWith ownerPKH
      ]

invalidDatumHarvestScriptContext :: ScriptContext
invalidDatumHarvestScriptContext =
  buildSpending' $
    mconcat
      [ txId harvestInTxId
      , input harvestInputUTXO
      , output invalidDatumHarvestOutputUTXO
      , withSpendingOutRefId harvestInTxId
      , signedWith ownerPKH
      ]

missSignatureHarvestScriptContext :: ScriptContext
missSignatureHarvestScriptContext =
  buildSpending' $
    mconcat
      [ txId harvestInTxId
      , input harvestInputUTXO
      , output harvestOutputUTXO
      , withSpendingOutRefId harvestInTxId
      ]

randomOutputUTXO :: UTXO
randomOutputUTXO =
  mconcat
    [withValue (singleton "" "" 2_000_000)]

incorrectOwnIndexHarvestScriptContext :: ScriptContext
incorrectOwnIndexHarvestScriptContext =
  buildSpending' $
    mconcat
      [ txId harvestInTxId
      , input harvestInputUTXO
      , output randomOutputUTXO
      , output harvestOutputUTXO
      , withSpendingOutRefId harvestInTxId
      , signedWith ownerPKH
      ]

addRewardsRedeemer :: YieldFarmRedeemer
addRewardsRedeemer = AddRewards {ownIndex = 0, authIndex = 1}

addRewardsInTxId :: TxId
addRewardsInTxId = TxId "2c6dbc95c1e96349c4131a9d19b029362542b31ffd2340ea85dd8f28e271ff6d"

addRewardsInputUTXO :: UTXO
addRewardsInputUTXO =
  mconcat
    [ address scriptAddress
    , withValue (singleton "" "" 5_000_000)
    , withInlineDatum datum
    ]

rewardToken :: Value
rewardToken = singleton "" "" 10_000_000

addRewardsOutputUTXO :: UTXO
addRewardsOutputUTXO =
  mconcat
    [ address scriptAddress
    , withValue (singleton "" "" 5_000_000 <> rewardToken)
    , withInlineDatum datum
    ]

authAddress :: Address
authAddress = Address (PubKeyCredential "e1317b152faac13426e6a83e06ff88a4d62cce3c1634ab0a5ec13309") Nothing

authToken :: Value
authToken = singleton currencySymbol tokenName 1

authInputUTXO :: UTXO
authInputUTXO =
  mconcat
    [ address authAddress
    , withValue (singleton "" "" 5_000_000 <> authToken)
    , withInlineDatum datum
    ]

addRewardScriptContext :: ScriptContext
addRewardScriptContext =
  buildSpending' $
    mconcat
      [ txId addRewardsInTxId
      , input addRewardsInputUTXO
      , input authInputUTXO
      , output addRewardsOutputUTXO
      , withSpendingOutRefId addRewardsInTxId
      ]

incorrectValueAddRewardsOutputUTXO :: UTXO
incorrectValueAddRewardsOutputUTXO =
  mconcat
    [ address scriptAddress
    , withValue (singleton "" "" 5_000_000)
    , withInlineDatum datum
    ]

incorrectOutputAddRewardScriptContext :: ScriptContext
incorrectOutputAddRewardScriptContext =
  buildSpending' $
    mconcat
      [ txId addRewardsInTxId
      , input addRewardsInputUTXO
      , input authInputUTXO
      , output incorrectValueAddRewardsOutputUTXO
      , withSpendingOutRefId addRewardsInTxId
      ]

incorrectAuthIndexAddRewardScriptContext :: ScriptContext
incorrectAuthIndexAddRewardScriptContext =
  buildSpending' $
    mconcat
      [ txId addRewardsInTxId
      , input authInputUTXO
      , input addRewardsInputUTXO
      , output addRewardsOutputUTXO
      , withSpendingOutRefId addRewardsInTxId
      ]

unitTest :: TestTree
unitTest = tryFromPTerm "Yield Farming Unit Test" (pvalidateYieldFarmW # pdata (pconstant currencySymbol) # pdata (pconstant tokenName)) $ do
  testEvalCase
    "Pass - Terminate Yield Farming"
    Success
    [ PlutusTx.toData datum
    , PlutusTx.toData terminateRedeemer
    , PlutusTx.toData terminateScriptContext
    ]
  testEvalCase
    "Failure - Terminate Yield Farming - missing signature"
    Failure
    [ PlutusTx.toData datum
    , PlutusTx.toData terminateRedeemer
    , PlutusTx.toData badTerminateScriptContext
    ]
  testEvalCase
    "Failure - Terminate Yield Farming - incorrect signature"
    Failure
    [ PlutusTx.toData datum
    , PlutusTx.toData terminateRedeemer
    , PlutusTx.toData badTerminateScriptContext2
    ]
  testEvalCase
    "Pass - Harvest Yield Farming"
    Success
    [ PlutusTx.toData datum
    , PlutusTx.toData harvestRedeemer
    , PlutusTx.toData harvestScriptContext
    ]
  testEvalCase
    "Failure - Harvest Yield Farming - incorrect output datum"
    Failure
    [ PlutusTx.toData datum
    , PlutusTx.toData harvestRedeemer
    , PlutusTx.toData invalidDatumHarvestScriptContext
    ]
  testEvalCase
    "Failure - Harvest Yield Farming - miss owner's signature"
    Failure
    [ PlutusTx.toData datum
    , PlutusTx.toData harvestRedeemer
    , PlutusTx.toData missSignatureHarvestScriptContext
    ]
  testEvalCase
    "Failure - Harvest Yield Farming - incorrect own index"
    Failure
    [ PlutusTx.toData datum
    , PlutusTx.toData harvestRedeemer
    , PlutusTx.toData incorrectOwnIndexHarvestScriptContext
    ]
  testEvalCase
    "Pass - Add Rewards Yield Farming"
    Success
    [ PlutusTx.toData datum
    , PlutusTx.toData addRewardsRedeemer
    , PlutusTx.toData addRewardScriptContext
    ]
  testEvalCase
    "Failure - Add Rewards Yield Farming - incorrect output value"
    Failure
    [ PlutusTx.toData datum
    , PlutusTx.toData addRewardsRedeemer
    , PlutusTx.toData incorrectOutputAddRewardScriptContext
    ]
  testEvalCase
    "Failure - Add Rewards Yield Farming - incorrect auth index"
    Failure
    [ PlutusTx.toData datum
    , PlutusTx.toData addRewardsRedeemer
    , PlutusTx.toData incorrectAuthIndexAddRewardScriptContext
    ]

-- Generator for a BuiltinByteString based on a subset of characters
genBuiltinByteString :: Gen BuiltinByteString
genBuiltinByteString = do
  member <- vectorOf 32 $ elements (['a' .. 'f'] ++ ['0' .. '9']) -- Hexadecimal representation
  return $ toBuiltin . pack $ member

genDatum :: Gen (YieldFarmingDatum, ScriptContext, ScriptContext)
genDatum = do
  cs <- CurrencySymbol <$> genBuiltinByteString
  tn <- TokenName <$> genBuiltinByteString
  pkh <- PubKeyHash <$> genBuiltinByteString
  fakepkh <- (PubKeyHash <$> genBuiltinByteString) `suchThat` (/= pkh)
  return
    ( YieldFarmingDatum
        { owner = Address (PubKeyCredential pkh) Nothing
        , lpCS = cs
        , lpTN = tn
        }
    , buildSpending' $
        mconcat
          [ signedWith pkh
          ]
    , buildSpending' $
        mconcat
          [ signedWith fakepkh
          ]
    )

prop_valid_terminate :: Property
prop_valid_terminate = forAll genDatum $ \(datum, ctx, _) ->
  fromPFun $
    pterminateYieldFarming # pconstant datum # pconstant ctx

prop_invalid_terminate :: Property
prop_invalid_terminate = forAll genDatum $ \(datum, _, invalidCtx) ->
  fromPFun $
    pnot #$ pterminateYieldFarming # pconstant datum # pconstant invalidCtx

yieldFarmingProperties :: TestTree
yieldFarmingProperties =
  testGroup
    "Yield Farming Properties"
    [ testProperty "True when valid terminate" prop_valid_terminate
    , testProperty "False when invalid terminate" prop_invalid_terminate
    ]
