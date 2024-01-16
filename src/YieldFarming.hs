{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module YieldFarming (
  pvalidateYieldFarmW,
  YieldFarmingDatum (..),
  YieldFarmRedeemer (..),
) where

import Plutarch.Api.V1 (PCredential (PPubKeyCredential, PScriptCredential))
import Plutarch.Api.V1.Value
import Plutarch.Api.V2
import Plutarch.Bool
import Plutarch.DataRepr
import Plutarch.Extra.ScriptContext (pfromPDatum, ptryFromInlineDatum)
import Plutarch.Prelude
import PlutusLedgerApi.V2 (
  Address,
  BuiltinByteString,
  CurrencySymbol,
  TokenName,
 )
import PlutusTx qualified
import "liqwid-plutarch-extra" Plutarch.Extra.TermCont

-- import qualified PlutusTx

import Plutarch.Lift (PConstantDecl, PUnsafeLiftDecl (PLifted))
import Plutarch.Unsafe (punsafeCoerce)

pcountOfUniqueTokens ::
  forall
    (keys :: KeyGuarantees)
    (amounts :: AmountGuarantees)
    (s :: S).
  Term s (PValue keys amounts :--> PInteger)
pcountOfUniqueTokens = phoistAcyclic $
  plam $ \val ->
    let tokensLength = plam (\pair -> pmatch (pfromData $ psndBuiltin # pair) $ \(PMap tokens) -> plength # tokens)
     in pmatch val $ \(PValue val') ->
          pmatch val' $ \(PMap csPairs) -> pfoldl # plam (\acc x -> acc + (tokensLength # x)) # 0 # csPairs

data YieldFarmingDatum = YieldFarmingDatum
  { owner :: Address
  , lpCS :: CurrencySymbol
  , lpTN :: TokenName
  }
  deriving stock (Generic, Eq, Show)

PlutusTx.makeIsDataIndexed ''YieldFarmingDatum [('YieldFarmingDatum, 0)]

newtype PYieldFarmDatum (s :: S)
  = PYieldFarmDatum
      ( Term
          s
          ( PDataRecord
              '[ "owner" ':= PAddress
               , "lpCS" ':= PCurrencySymbol
               , "lpTN" ':= PTokenName
               ]
          )
      )
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PDataFields)

instance DerivePlutusType PYieldFarmDatum where
  type DPTStrat _ = PlutusTypeData

instance PTryFrom PData PYieldFarmDatum

instance PUnsafeLiftDecl PYieldFarmDatum where type PLifted PYieldFarmDatum = YieldFarmingDatum
deriving via (DerivePConstantViaData YieldFarmingDatum PYieldFarmDatum) instance PConstantDecl YieldFarmingDatum

data YieldFarmRedeemer
  = Terminate
  | HarvestRewards {ownIndex :: Integer}
  | AddRewards {ownIndex :: Integer, authIndex :: Integer}
  deriving stock (Show, Eq, Generic)

PlutusTx.makeIsDataIndexed
  ''YieldFarmRedeemer
  [ ('Terminate, 0)
  , ('HarvestRewards, 1)
  , ('AddRewards, 2)
  ]
PlutusTx.makeLift ''YieldFarmRedeemer

data PYieldFarmRedeemer (s :: S)
  = PTerminate (Term s (PDataRecord '[]))
  | PHarvestRewards (Term s (PDataRecord '["ownIndex" ':= PInteger]))
  | PAddRewards (Term s (PDataRecord '["ownIndex" ':= PInteger, "authIndex" ':= PInteger]))
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData)

instance DerivePlutusType PYieldFarmRedeemer where
  type DPTStrat _ = PlutusTypeData

instance PTryFrom PData PYieldFarmRedeemer

instance PUnsafeLiftDecl PYieldFarmRedeemer where type PLifted PYieldFarmRedeemer = YieldFarmRedeemer
deriving via (DerivePConstantViaData YieldFarmRedeemer PYieldFarmRedeemer) instance PConstantDecl YieldFarmRedeemer

ptryOwnInput :: (PIsListLike list PTxInInfo) => Term s (list PTxInInfo :--> PTxOutRef :--> PTxOut)
ptryOwnInput = phoistAcyclic $
  plam $ \inputs ownRef ->
    precList
      ( \self x xs ->
          pletFields @'["outRef", "resolved"] x $ \txInFields ->
            pif (ownRef #== txInFields.outRef) txInFields.resolved (self # xs)
      )
      (const perror)
      # inputs

-- pelimList Arguments:
-- 1. function that returns something, runs when not empty
-- 2. nilCase -> what happens when list is empty
-- 3. list to recurse on
pheadSingleton :: (PListLike list, PElemConstraint list a) => Term s (list a :--> a)
pheadSingleton = phoistAcyclic $
  plam $ \xs ->
    pelimList (\x xs -> (pelimList (\_ _ -> perror) x xs)) perror xs

pterminateYieldFarming :: Term s (PYieldFarmDatum :--> PScriptContext :--> PBool)
pterminateYieldFarming = phoistAcyclic $ plam $ \datum ctx -> unTermCont $ do
  ctxF <- pletFieldsC @'["txInfo"] ctx
  txInfo <- pletFieldsC @'["signatories"] ctxF.txInfo
  PPubKeyCredential ((pfield @"_0" #) -> ownerPkh) <- pmatchC $ pfield @"credential" # (pfield @"owner" # datum)
  pure $ pelem # ownerPkh # txInfo.signatories

pharvestYieldFarm ::
  Term s (PInteger :--> PYieldFarmDatum :--> PScriptContext :--> PBool)
pharvestYieldFarm = phoistAcyclic $ plam $ \ownIndex oldDatum ctx ->
  unTermCont $ do
    ctxF <- pletFieldsC @'["txInfo", "purpose"] ctx
    txInfo <- pletFieldsC @'["inputs", "outputs", "signatories"] ctxF.txInfo
    PSpending ((pfield @"_0" #) -> ownRef) <- pmatchC ctxF.purpose

    ownInput <- pletFieldsC @'["address", "datum"] (ptryOwnInput # txInfo.inputs # ownRef)
    ownOutput <- pletFieldsC @'["address", "datum"] (pelemAt @PBuiltinList # ownIndex # txInfo.outputs)
    datum <- pletFieldsC @'["owner", "lpCS", "lpTN"] oldDatum
    ownerAddress <- pletC datum.owner

    let signedByOwner =
          pmatch (pfromData $ pfield @"credential" # ownerAddress) $ \case
            PPubKeyCredential hsd -> pelem # (pfield @"_0" # hsd) # txInfo.signatories
            PScriptCredential _ -> perror
        correctOutput = ownOutput.address #== ownInput.address #&& ownOutput.datum #== ownInput.datum
    pure (signedByOwner #&& correctOutput)

paddYieldFarmRewards ::
  Term s (PCurrencySymbol :--> PTokenName :--> PInteger :--> PInteger :--> PScriptContext :--> PBool)
paddYieldFarmRewards = phoistAcyclic $ plam $ \batcherCS batcherTN ownIndex authTokenIndex ctx -> unTermCont $ do
  ctxF <- pletFieldsC @'["txInfo", "purpose"] ctx

  txInfo <- pletFieldsC @'["inputs", "outputs", "referenceInputs", "validRange"] ctxF.txInfo
  txInputs <- pletC txInfo.inputs

  indexedInput <- pletFieldsC @'["outRef", "resolved"] (pelemAt @PBuiltinList # ownIndex # txInputs)

  PSpending ((pfield @"_0" #) -> ownRef) <- pmatchC ctxF.purpose

  ownInput <- pletFieldsC @'["datum", "value", "address"] indexedInput.resolved
  ownOutput <- pletFieldsC @'["datum", "value", "address"] (pelemAt # ownIndex # pfromData txInfo.outputs)

  let correctIdx = ownRef #== indexedInput.outRef
      correctOutput = ownInput.datum #== ownOutput.datum #&& pfromData ownInput.value #< pfromData ownOutput.value #&& ownInput.address #== ownOutput.address #&& (pcountOfUniqueTokens # ownOutput.value) #<= 5
      hasAuth = pvalueOf # (pfield @"value" # (pfield @"resolved" # (pelemAt @PBuiltinList # authTokenIndex # txInputs))) # batcherCS # batcherTN #== 1

  pure (correctIdx #&& hasAuth #&& correctOutput)

pvalidateYieldFarm :: Term s (PCurrencySymbol :--> PTokenName :--> PYieldFarmDatum :--> PYieldFarmRedeemer :--> PScriptContext :--> PBool)
pvalidateYieldFarm = phoistAcyclic $ plam $ \batcherCS batcherTN datum redeemer ctx -> pmatch redeemer $ \case
  PTerminate _ -> pterminateYieldFarming # datum # ctx
  PAddRewards red -> pletFields @'["ownIndex", "authIndex"] red $ \redF ->
    paddYieldFarmRewards # batcherCS # batcherTN # redF.ownIndex # redF.authIndex # ctx
  PHarvestRewards r -> pharvestYieldFarm # (pfield @"ownIndex" # r) # datum # ctx
pvalidateYieldFarmW :: Term s (PAsData PCurrencySymbol :--> PAsData PTokenName :--> PValidator)
pvalidateYieldFarmW = phoistAcyclic $ plam $ \batcherCS batcherTN datum redeemer ctx ->
  let dat :: Term _ PYieldFarmDatum
      dat = punsafeCoerce datum
      red :: Term _ PYieldFarmRedeemer
      red = punsafeCoerce redeemer
   in pif (pvalidateYieldFarm # pfromData batcherCS # pfromData batcherTN # dat # red # ctx) (popaque $ pconstant ()) perror
