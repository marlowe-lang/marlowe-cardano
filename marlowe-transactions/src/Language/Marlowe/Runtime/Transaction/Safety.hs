module Language.Marlowe.Runtime.Transaction.Safety (
  Continuations,
  ThreadTokenAssetId (..),
  checkContract,
  checkTransactions,
  makeSystemHistory,
  mkLockedRolesContext,
  mockLockedRolesContext,
  minAdaUpperBound,
  mkAdjustMinUTxO,
  noContinuations,
) where

import Control.Monad (foldM, when)
import Control.Monad.Trans.Except (except, runExceptT)
import Data.Bifunctor (bimap, first)
import Data.Maybe (catMaybes, fromMaybe, isJust)
import Data.SOP.BasicFunctors (K (..))
import Data.SOP.Strict (NP (..))
import Data.String (fromString)
import Data.Time (UTCTime, addUTCTime, secondsToNominalDiffTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Marlowe.Plutus.Analysis.Safety.Ledger (
  checkAddresses,
  checkContinuations,
  checkNetwork,
  checkNetworks,
  checkRoleNames,
  checkTokens,
  worstValue,
 )
import Marlowe.Plutus.Analysis.Safety.Transaction (
  CurrentState (AlreadyInitialized),
  findTransactions,
  roleAuthorizations,
  unitAnnotator,
 )
import Marlowe.Plutus.Analysis.Safety.Types (SafetyError (..), Transaction (..), stripAnnotation)
import Language.Marlowe.Runtime.Core.Api (
  Contract,
  Datum,
  MarloweTransactionMetadata (..),
  MarloweVersion (MarloweV1),
  TransactionScriptOutput (..),
 )
import Language.Marlowe.Runtime.Transaction.Api (Mint (..), RoleTokensConfig (..))
import Language.Marlowe.Object.Types (ContractHash (ContractHash))
import Language.Marlowe.Runtime.Transaction.BuildConstraints (
  AdjustMinUTxO (..),
  ThreadTokenAssetId (..),
  buildApplyInputsConstraints,
  marloweAccountsAssets,
  safeLovelace,
 )
import Language.Marlowe.Runtime.Transaction.Constraints (
  HelperScriptInfo (helperAddress),
  HelperScriptState (..),
  HelpersContext (..),
  MarloweContext (..),
  MarloweOutputConstraints (..),
  RoleTokenConstraints (..),
  TxConstraints (..),
  WalletContext (..),
  mkTxOutValue,
  solveConstraints,
 )

import qualified Cardano.Api as Cardano
import qualified Cardano.Api as C
import Control.Error (note)
import Control.Monad.Trans.State.Strict (evalState, get, put)
import Data.Functor.Identity (runIdentity)
import Data.List (nub)
import qualified Data.Map as Map
import qualified Data.Map.NonEmpty as NEMap
import qualified Data.Map.Strict as M (
  Map,
  elems,
  empty,
  fromList,
  keysSet,
  mapKeys,
  singleton,
  size,
  traverseWithKey,
 )
import qualified Data.SOP.Counting as Ouroboros
import qualified Data.Set as Set
import qualified Data.Set.NonEmpty as NESet
import qualified Marlowe.Plutus.Merkle as V1 (MerkleizedContract (..))
import qualified Marlowe.Plutus.Plate as V1 (extractRoleNames)
import qualified Marlowe.Plutus.Semantics as V1 (
  MarloweData (..),
  MarloweParams (..),
  TransactionInput (..),
  TransactionOutput (..),
 )
import qualified Marlowe.Plutus.Semantics.Types as V1 (State (..))
import Language.Marlowe.Runtime.Cardano.Api (fromCardanoLovelace, cardanoEraToAsType)
import qualified Language.Marlowe.Runtime.Cardano.Api as Chain (
  assetsToCardanoValue,
  fromCardanoDatumHash,
  toCardanoScriptData,
 )
import Language.Marlowe.Runtime.ChainSync.Api (mkTxOutAssets, subtractTxOutAssetsRounding, unsafeTxOutAssets)
import qualified Language.Marlowe.Runtime.ChainSync.Api as CS
import qualified Language.Marlowe.Runtime.ChainSync.Api as Chain (
  Address (..),
  AssetId (..),
  Assets (..),
  Datum (B),
  Lovelace,
  PolicyId (..),
  SlotNo (..),
  TokenName (..),
  Tokens (..),
  TransactionMetadata (..),
  TransactionOutput (..),
  TxOutAssets,
  TxOutRef,
  UTxOs (..),
 )
import qualified Language.Marlowe.Runtime.Plutus.V2.Api as Chain (
  fromPlutusCurrencySymbol,
  fromPlutusTokenName,
  fromPlutusValue,
  toPlutusTokenName,
 )
import Ouroboros.Consensus.Block (GenesisWindow (GenesisWindow))
import qualified Ouroboros.Consensus.Block as Ouroboros
import qualified Ouroboros.Consensus.BlockchainTime as Ouroboros (RelativeTime (..), mkSlotLength, toRelativeTime)
import Ouroboros.Consensus.HardFork.History (EraParams (eraGenesisWin), pattern NoPerasEnabled)
import qualified Ouroboros.Consensus.HardFork.History as Ouroboros (
  Bound (..),
  EraEnd (..),
  EraParams (..),
  EraSummary (..),
  SafeZone (..),
  interpretQuery,
  mkInterpreter,
  summaryWithExactly,
  wallclockToSlot,
 )
import qualified PlutusLedgerApi.V2 as Plutus (
  CurrencySymbol (..),
  DatumHash (..),
  POSIXTime (..),
  toBuiltin,
 )
import qualified Cardano.Ledger.Core as Ledger

-- FIXME: Relocate this definition when full support for Merkleization is added to Runtime.
type Continuations v = M.Map ContractHash (Contract v)

-- Roles still locked in the script on the chain. All the `helperUTxO` are non empty.
-- We could use parameter to enforce th in the original `HelpersContext` but it would overcomplicate the code all over the place.
newtype LockedRolesContext = LockedRolesContext HelpersContext

-- We check if script states have non empty helperUTxO.
mkLockedRolesContext :: HelpersContext -> LockedRolesContext
mkLockedRolesContext (HelpersContext helperScripts policyId helperScriptStates) = do
  let helperScriptStates' = Map.filter (\HelperScriptState{helperUTxO} -> isJust helperUTxO) helperScriptStates
  LockedRolesContext $ HelpersContext helperScripts policyId helperScriptStates'

-- For every empty `helperUTxO` provide mock txOut info.
mockLockedRolesContext
  :: ThreadTokenAssetId
  -> AdjustMinUTxO
  -> HelpersContext
  -> LockedRolesContext
mockLockedRolesContext (ThreadTokenAssetId threadTokenAssetId) (AdjustMinUTxO adjustMinUtxo) hc@(HelpersContext _ _ helperScriptStates) = do
  let Chain.AssetId threadTokenPolicyId threadTokenName = threadTokenAssetId
      helperScriptStates' :: M.Map Chain.TokenName HelperScriptState
      helperScriptStates' = flip evalState (1 :: Integer) $ flip M.traverseWithKey helperScriptStates \roleName v -> case v of
        s@(HelperScriptState _ (Just _)) -> pure s
        HelperScriptState helperScriptInfo _ -> do
          let -- We assume that roles have the same policy id as the thread token which can change in the future.
              roleTokenAssetId = Chain.AssetId threadTokenPolicyId roleName
          ix <- get
          put (ix + 1)
          let txOutRef = fromString ("EEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEE#" <> show ix)
              address = helperAddress helperScriptInfo
              assets =
                adjustMinUtxo . unsafeTxOutAssets . Chain.Assets safeLovelace . Chain.Tokens $
                  M.fromList [(roleTokenAssetId, CS.Quantity 1)]
              datum = Just $ Chain.B $ Chain.unTokenName threadTokenName
              datumHash =
                Chain.fromCardanoDatumHash
                  . C.hashScriptDataBytes
                  . C.unsafeHashableScriptData
                  . Chain.toCardanoScriptData
                  <$> datum
              helperTransactionOutput = Chain.TransactionOutput{..}
          pure $ HelperScriptState helperScriptInfo $ Just (txOutRef, helperTransactionOutput)
  LockedRolesContext $ hc{helperScriptStates = helperScriptStates'}

unlockRoles :: Set.Set Chain.TokenName -> LockedRolesContext -> LockedRolesContext
unlockRoles rolesToUnlock (LockedRolesContext (HelpersContext helperScripts policyId helperScriptStates)) = do
  let helperScriptStates' = Map.filterWithKey (\k _ -> k `Set.notMember` rolesToUnlock) helperScriptStates
  LockedRolesContext $ HelpersContext helperScripts policyId helperScriptStates'

-- | No continuations for a contract.
noContinuations
  :: MarloweVersion v
  -> Continuations v
noContinuations MarloweV1 = M.empty

-- | Map Plutus continuations into chain-sync continuations.
remapContinuations
  :: M.Map ContractHash contract
  -> M.Map Plutus.DatumHash contract
remapContinuations = do
  let toPlutusDatumHash (ContractHash bytes) = Plutus.DatumHash . Plutus.toBuiltin $ bytes
  M.mapKeys toPlutusDatumHash

-- | Compute a worst-case bound on the minimum UTxO value for a contract, assuming that the contract does not pay
--   from the account in the initial state and that account only contains lovelace. Assume that a datum hash is
--   present in the TxOut and that the address contains a stake credential.
minAdaUpperBound
  :: forall era v
   . C.BabbageEraOnwards era
  -> Ledger.PParams (C.ShelleyLedgerEra era)
  -> MarloweVersion v
  -> V1.State
  -> Contract v
  -> Continuations v
  -> Maybe Chain.Lovelace
minAdaUpperBound era pps MarloweV1 state contract continuations = do
  assets <-
    mkTxOutAssets
      . Chain.fromPlutusValue
      . worstValue (Just state) contract
      $ remapContinuations continuations
  minAdaBound era pps MarloweV1 assets

-- | Adjust the ada in a TxOut upwards, if needed to satisfy the minimum UTxO ledger rule. Assume that a datum hash is
-- present in the TxOut and that the address contains a stake credential.
mkAdjustMinUTxO
  :: forall era v
   . C.BabbageEraOnwards era
  -> Ledger.PParams (C.ShelleyLedgerEra era)
  -> MarloweVersion v
  -> AdjustMinUTxO
mkAdjustMinUTxO era pps MarloweV1 =
  AdjustMinUTxO \assets@(CS.TxOutAssetsContent (Chain.Assets{ada, tokens})) -> do
    let minLovelace =
          maybe (CS.Lovelace 30_000_000) id $ -- Safe for all UTxOs.
            minAdaBound era pps MarloweV1 assets
    fromMaybe assets $
      if minLovelace > ada
        then mkTxOutAssets $ Chain.Assets minLovelace tokens
        else Nothing


-- | Compute a worst-case bound on the minimum TxOut value for assets. Assume that a datum hash is present in the
--   TxOut and that the address contains a stake credential.
minAdaBound
  :: forall era v
   . C.BabbageEraOnwards era
  -- -> C.LedgerProtocolParameters era
  -> Ledger.PParams (C.ShelleyLedgerEra era)
  -> MarloweVersion v
  -> Chain.TxOutAssets
  -> Maybe Chain.Lovelace
minAdaBound eraOnwards pps MarloweV1 assets = C.babbageEraOnwardsConstraints eraOnwards $ do
  let
    era = C.cardanoEra @era
    addrStr = "addr1x8ur42seq20ytdzm33fhjf3c2pxskxf3gzxq706qk7p5muhc824pjq57gk69hrzn0ynrs5zdpvvnzsyvpul5pdurfheq28w2dx"
  address <- C.deserialiseAddress (C.AsAddressInEra $ cardanoEraToAsType era) addrStr
  let -- Let's just check if cardano-node fixed the issue
      CS.TxOutAssetsContent (Chain.Assets origLovelace tokens) = assets
      minAdaLoop counter totalLovelace = do
        when (counter == (0 :: Int)) do
          Nothing
        let assets' = unsafeTxOutAssets (Chain.Assets totalLovelace tokens)
        value <- Chain.assetsToCardanoValue assets'
        let
          txOut =
            Cardano.TxOut
              address
              (mkTxOutValue (C.convert eraOnwards) value)
              (C.TxOutDatumInline (C.convert eraOnwards) . C.unsafeHashableScriptData . Chain.toCardanoScriptData $ Chain.B "")
              C.ReferenceScriptNone
          minUTxOLovelace =
            fromCardanoLovelace $
              C.calculateMinimumUTxO
                (C.convert eraOnwards)
                pps
                txOut
        if minUTxOLovelace > totalLovelace
          then minAdaLoop (counter - 1) minUTxOLovelace
          else pure minUTxOLovelace
  minAdaLoop 5 origLovelace

-- | Check a contract for design errors and ledger violations.
checkContract
  :: Cardano.NetworkId
  -> Maybe RoleTokensConfig
  -- FIXME: This should depend on the context:
  --  * it could be the config used to perform minting
  --  * it could be minting information gathered from the chain
  --
  --  Currently we handle only the first case.
  -> MarloweVersion v
  -> Datum v
  -> Continuations v
  -> [SafetyError]
checkContract network roleTokensConfig MarloweV1 marloweData continuations =
  let continuations' = remapContinuations continuations
      V1.MarloweData{marloweState = state, marloweParams = V1.MarloweParams{rolesCurrency}, marloweContract = contract} = marloweData
      mintCheck = flip foldMap roleTokensConfig \roleTokensConfig' ->
        checkMinting roleTokensConfig' MarloweV1 contract state continuations
      rolesCheck = checkRoleNames rolesCurrency state contract continuations'
      tokenCheck = checkTokens state contract continuations'
      continuationCheck = checkContinuations contract continuations'
      networksCheck =
        checkNetwork (network == Cardano.Mainnet) state contract continuations'
          <> snd (checkNetworks state contract continuations')
      addressCheck = checkAddresses state contract continuations'
   in nub $ mintCheck <> rolesCheck <> tokenCheck <> continuationCheck <> networksCheck <> addressCheck

checkMinting
  :: RoleTokensConfig
  -> MarloweVersion v
  -> Contract v
  -> V1.State
  -> Continuations v
  -> [SafetyError]
checkMinting config MarloweV1 contract state continuations = do
  let continuations' = remapContinuations continuations
      roles = V1.extractRoleNames state contract continuations'
  case (config, Set.null roles) of
    (RoleTokensNone, False) -> pure MissingRolesCurrency
    (RoleTokensNone, True) -> mempty
    (_, True) -> pure ContractHasNoRoles
    (RoleTokensMint mint, False) ->
      let minted = Set.map Chain.toPlutusTokenName $ NESet.toSet $ NEMap.keysSet $ unMint mint
          missing = MissingRoleToken <$> Set.toList (Set.difference roles minted)
          extra = ExtraRoleToken <$> Set.toList (Set.difference minted roles)
       in missing <> extra
    (RoleTokensUsePolicy _ distribution, False) -> do
      let distributedRoles = Set.map Chain.toPlutusTokenName $ M.keysSet distribution
      extraRole <- Set.toList $ Set.difference distributedRoles roles
      pure $ ExtraRoleToken extraRole

-- | Mock-execute all possible transactions for a contract.
checkTransactions
  :: Ledger.PParams (C.ShelleyLedgerEra era)
  -> C.BabbageEraOnwards era
  -> MarloweVersion v
  -> MarloweContext v
  -> LockedRolesContext
  -> Chain.Address
  -> Datum v
  -> Continuations v
  -> IO (Either String [SafetyError])
checkTransactions protocolParameters era version@MarloweV1 marloweContext lockedRolesContext changeAddress datum continuations =
  runExceptT $
    do
      let V1.MarloweData{marloweState = state, marloweParams = params, marloweContract = contract} = datum
          V1.MarloweParams{rolesCurrency} = params
          rolesCurrency' = Chain.fromPlutusCurrencySymbol rolesCurrency
      transactions <- do
        let state' = AlreadyInitialized state
            contract' = V1.MerkleizedContract contract $ remapContinuations continuations
        findTransactions unitAnnotator False contract' state'
      let foldMFlipped :: (Foldable t, Monad m) => b -> t a -> (b -> a -> m b) -> m b
          foldMFlipped b ta f = foldM f b ta

      (_, res) <- except $ foldMFlipped (lockedRolesContext, []) transactions \(lockedRolesCtx, acc) tx@(Transaction{txInput}) -> do
        let usedRoles = roleAuthorizations txInput
            lockedRolesCtx' = unlockRoles (Set.map Chain.fromPlutusTokenName usedRoles) lockedRolesCtx

        (txRes :: [SafetyError]) <-
          checkTransaction
            protocolParameters
            era
            version
            marloweContext
            lockedRolesCtx
            rolesCurrency'
            changeAddress
            tx
        pure (lockedRolesCtx', txRes : acc)

      pure $ concat res

plainInterval
  :: Plutus.POSIXTime
  -> (Plutus.POSIXTime, Plutus.POSIXTime)
  -> (UTCTime, UTCTime, UTCTime)
plainInterval minTime marloweInterval = do
  let beginPOSIX = fst marloweInterval
      begin = posixTimeToUTCTime beginPOSIX
      -- We have to revert back from the `MarloweTimeInterval` to the TxInterval:
      --
      -- \* on the Plutus side we do this millisecond subtraction to compute
      --  the end of Marlowe level interval based on tx level interval:
      -- https://github.com/input-output-hk/marlowe-plutus/blob/d9c3093270e7af8b335ee2ddce9c0a18be2bd9a3/marlowe-plutus/src/Language/Marlowe/Plutus/Script.hs#L190
      --
      -- \* we have to do the opposite here:
      endPOSIX = snd marloweInterval + 1
      end = posixTimeToUTCTime endPOSIX
      minTime' = posixTimeToUTCTime $ max minTime beginPOSIX
  (minTime', begin, end)

-- | Check a transaction for safety issues.
checkTransaction
  :: Ledger.PParams (C.ShelleyLedgerEra era)
  -> C.BabbageEraOnwards era
  -> MarloweVersion v
  -> MarloweContext v
  -> LockedRolesContext
  -> Chain.PolicyId
  -> Chain.Address
  -> Transaction a
  -> Either String [SafetyError]
checkTransaction protocolParameters era version@MarloweV1 marloweContext@MarloweContext{..} lockedRolesContext (Chain.PolicyId rolesCurrency) changeAddress transaction@Transaction{..} =
  do
    let V1.TransactionInput{..} = txInput
        rolesCurrency' = V1.MarloweParams . Plutus.CurrencySymbol $ Plutus.toBuiltin rolesCurrency
        marloweData = V1.MarloweData rolesCurrency' txState txContract
        minTime = V1.minTime txState
        (now, intervalBegin, intervalEnd) = plainInterval minTime txInterval

    scriptIncoming <-
      note "Invalid initial accounts state" $ marloweAccountsAssets . V1.accounts $ V1.marloweState marloweData
    let marloweOutput = TransactionScriptOutput marloweAddress scriptIncoming scriptTxOutRef marloweData
        marloweContext' = marloweContext{scriptOutput = Just marloweOutput}
        metadata = MarloweTransactionMetadata Nothing $ Chain.TransactionMetadata mempty
        (start, history) =
          makeSystemHistory . posixTimeToUTCTime $ Plutus.POSIXTime 0
        solveConstraints' = solveConstraints start (C.toLedgerEpochInfo history)
    tipSlot <-
      utcTimeToSlotNo start history now
    constraints <-
      bimap show snd $
        runIdentity $ do
          runExceptT $
            buildApplyInputsConstraints
              (const $ pure Nothing)
              start
              history
              version
              marloweOutput
              tipSlot
              metadata
              (Just intervalBegin)
              (Just intervalEnd)
              txInputs

    let
      walletContext = walletForConstraints version marloweContext changeAddress constraints
      LockedRolesContext helpersContext = lockedRolesContext
    pure
      . either
        (pure . TransactionValidationError (stripAnnotation transaction) . show)
        (const $ TransactionWarning (stripAnnotation transaction) <$> V1.txOutWarnings txOutput)
      $ solveConstraints' era protocolParameters version (Left marloweContext') walletContext helpersContext constraints

-- | Create a wallet context that will satisfy the given constraints.
walletForConstraints
  :: MarloweVersion v
  -> MarloweContext v
  -> Chain.Address
  -> TxConstraints era v
  -> WalletContext
walletForConstraints MarloweV1 MarloweContext{scriptOutput} changeAddress TxConstraints{..} =
  let mkTxOutAssets' = fromMaybe mempty . mkTxOutAssets
      lovelaceAssets
        :: Integer
        -> Chain.TxOutAssets
      lovelaceAssets quantity = mkTxOutAssets' $ Chain.Assets (CS.Lovelace quantity) $ Chain.Tokens M.empty

      padValue assets@(CS.TxOutAssetsContent (Chain.Assets _ (Chain.Tokens tokens))) =
        assets <> lovelaceAssets (toInteger $ 2_000_000 * M.size tokens)

      mkTransactionOutput addr value = do
        let padded = padValue value
        if padded == mempty
          then Nothing
          else Just $ Chain.TransactionOutput addr padded Nothing Nothing
      mkUtxo txOutRef addr value = (txOutRef,) <$> mkTransactionOutput addr value

      scriptIncoming = maybe mempty (\(TransactionScriptOutput _ assets _ _) -> assets) scriptOutput
      scriptOutgoing =
        case marloweOutputConstraints of
          MarloweOutput assets _ -> assets
          _ -> mempty
      roles =
        case roleTokenConstraints of
          SpendRoleTokens assets -> mkTxOutAssets' $ foldMap (Chain.Assets mempty . Chain.Tokens . flip M.singleton (CS.Quantity 1)) assets
          _ -> mempty
      payments = mconcat $ M.elems payToAddresses <> M.elems payToRoles
      bufferValue = lovelaceAssets 50_000_000 -- Generously cover min-UTxO and fee.
      workaround = True
      collateralUtxos = Set.singleton collateralTxOutRef
      availableUtxos =
        Chain.UTxOs $
          M.fromList $
            if workaround
              then do
                -- FIXME: Workaround for coin-selection bug.
                let requiredScriptAssets = scriptOutgoing `subtractTxOutAssetsRounding` scriptIncoming
                catMaybes
                  [ mkUtxo collateralTxOutRef changeAddress bufferValue
                  , mkUtxo (fundingTxOutRef 0) changeAddress roles
                  , mkUtxo (fundingTxOutRef 1) changeAddress payments
                  , mkUtxo (fundingTxOutRef 2) changeAddress requiredScriptAssets
                  ]
              else do
                let requiredValue = (roles <> payments <> scriptOutgoing) `subtractTxOutAssetsRounding` scriptIncoming
                catMaybes
                  [ mkUtxo collateralTxOutRef changeAddress bufferValue
                  , mkUtxo (fundingTxOutRef 0) changeAddress requiredValue
                  ]
   in WalletContext{..}

-- | Convert UTC time to a slot number.
utcTimeToSlotNo
  :: C.SystemStart
  -> C.EraHistory
  -> UTCTime
  -> Either String Chain.SlotNo
utcTimeToSlotNo systemStart (C.EraHistory interpreter) time =
  do
    (slotNo, _, _) <-
      first show
        . Ouroboros.interpretQuery interpreter
        . Ouroboros.wallclockToSlot
        $ Ouroboros.toRelativeTime systemStart time
    pure . Chain.SlotNo $ Ouroboros.unSlotNo slotNo

-- | Convert POSIX time to UTC time.
posixTimeToUTCTime
  :: Plutus.POSIXTime
  -> UTCTime
posixTimeToUTCTime (Plutus.POSIXTime t) = posixSecondsToUTCTime . secondsToNominalDiffTime $ fromInteger t / 1000

-- | Make a system start and error history for the given start time.
makeSystemHistory
  :: UTCTime
  -> (C.SystemStart, C.EraHistory)
makeSystemHistory time =
  let systemStart = C.SystemStart $ addUTCTime (-1000) time
      eraParams =
        Ouroboros.EraParams
          { eraEpochSize = Ouroboros.EpochSize 1
          , eraSlotLength = Ouroboros.mkSlotLength 1
          , eraSafeZone = Ouroboros.UnsafeIndefiniteSafeZone
          , eraGenesisWin = GenesisWindow 1
          , eraPerasRoundLength = NoPerasEnabled
          }
      oneSecondBound i =
        Ouroboros.Bound
          { boundTime = Ouroboros.RelativeTime $ fromInteger i
          , boundSlot = Ouroboros.SlotNo $ fromInteger i
          , boundEpoch = Ouroboros.EpochNo $ fromInteger i
          , boundPerasRound = NoPerasEnabled
          }
      oneSecondEraSummary i =
        Ouroboros.EraSummary
          { eraStart = oneSecondBound i
          , eraEnd = Ouroboros.EraEnd . oneSecondBound $ i + 1
          , eraParams
          }
      unboundedEraSummary i =
        Ouroboros.EraSummary
          { eraStart = oneSecondBound i
          , eraEnd = Ouroboros.EraUnbounded
          , eraParams
          }
      eraHistory =
        C.EraHistory
          . Ouroboros.mkInterpreter
          . Ouroboros.summaryWithExactly
          . Ouroboros.Exactly
          $ K (oneSecondEraSummary 0) -- Byron lasted 1 second
            :* K (oneSecondEraSummary 1) -- Shelley lasted 1 second
            :* K (oneSecondEraSummary 2) -- Allegra lasted 1 second
            :* K (oneSecondEraSummary 3) -- Mary lasted 1 second
            :* K (oneSecondEraSummary 4) -- Alonzo lasted 1 second
            :* K (oneSecondEraSummary 5) -- Babbage lasted 1 second
            :* K (unboundedEraSummary 6) -- Conway lasted 1 second
            :* K (unboundedEraSummary 7) -- Dijkstra never ends
            :* Nil
   in (systemStart, eraHistory)

-- | A dummy collateral UTxO.
collateralTxOutRef :: Chain.TxOutRef
collateralTxOutRef = "FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF#0"

-- | A dummy script reference.
scriptTxOutRef :: Chain.TxOutRef
scriptTxOutRef = "FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF#1"

-- | A dummy TxOut reference.
fundingTxOutRef :: Int -> Chain.TxOutRef
fundingTxOutRef = fromString . ("FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF#" <>) . show . (2 +)
