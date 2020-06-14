{-# LANGUAGE RecordWildCards #-}

module Plugin.InjTyFam (
  plugin,
  tcPlugin,
  ) where

import           Data.Maybe (fromMaybe)

import qualified Coercion
import qualified CoreSyn
import qualified Plugins
import           TcEvidence (EvTerm)
import qualified TcPluginM
import qualified TcRnMonad
import qualified TcRnTypes
import           TcRnTypes (Ct, Xi)
import           TcType (TcType)
import qualified TyCoRep
import           TyCoRep (Coercion)
import qualified TyCon
import           TyCon (TyCon)
import qualified Type
import           VarEnv (VarEnv)
import qualified VarEnv

-- | Just 'tcPlugin', with 'Plugins.purePlugin' set

plugin :: Plugins.Plugin
plugin = Plugins.defaultPlugin
    {
      Plugins.pluginRecompile = Plugins.purePlugin
    ,
      Plugins.tcPlugin = \_opts -> pure tcPlugin
    }

-- | A typechecker plugin that decomposes Given equalities headed by
-- an injective type family,
-- <https://downloads.haskell.org/ghc/8.6.5/docs/html/users_guide/glasgow_exts.html#injective-type-families>
--
-- The plugin user is " opting in " to
-- <https://gitlab.haskell.org/ghc/ghc/issues/10833>

tcPlugin :: TcRnTypes.TcPlugin
tcPlugin = TcRnTypes.TcPlugin
    {
      TcRnTypes.tcPluginInit = pure ()
    ,
      TcRnTypes.tcPluginSolve = \() gs ds ws -> do
        let
          fskEnv :: FskEnv
          fskEnv = mkFskEnv gs

        (uncurry TcRnTypes.TcPluginOk . mconcat) <$>
          if null ds && null ws
          then mapM (simplifyG fskEnv) gs   -- we handle G-mode
          else pure []   -- GHC already handles W-mode
    ,
      TcRnTypes.tcPluginStop = \() -> pure ()
    }

-----

type M = TcPluginM.TcPluginM

-- | The arguments to 'TcRnTypes.TcPluginOk'

type Ok = ([(EvTerm, Ct)], [Ct])

-- | See 'tcPlugin'

simplifyG :: FskEnv -> Ct -> M Ok
simplifyG fskEnv ct =
    case predTree of
      Type.ClassPred{}  -> giveup   -- TODO don't need to unpack SCs, right?
      Type.IrredPred{}  -> giveup
      Type.ForAllPred{} -> giveup   -- TODO this is unreachable, right?
      Type.EqPred eqRel lty rty -> case eqRel of
          Type.ReprEq -> giveup   -- TODO anything useful to do here?
          Type.NomEq  -> case (,) <$> prj lty <*> prj rty of
              Nothing ->
                  giveup   -- one side isn't an injective tyfam application
              Just ((ltc, ltys), (rtc, rtys)) ->
                  if ltc /= rtc then giveup else do
                    -- both sides apply the same tyfam
                    news <- go [] ltys rtys
                    pure ([(ev, ct)], news)
  where
    ev :: EvTerm
    ev = TcRnTypes.ctEvTerm $ TcRnTypes.cc_ev ct

    prj :: TcType -> Maybe (TyCon, [Xi])
    prj = splitInjTyFamApp_maybe . unfsk fskEnv

    giveup :: M Ok
    giveup = pure ([], [])

    predTree :: Type.PredTree
    predTree = Type.classifyPredType (TcRnTypes.ctPred ct)

    go :: [Ct] -> [TcType] -> [TcType] -> M [Ct]
    go acc []         []         = pure acc
    go acc (lty:ltys) (rty:rtys) = do
        new <- impliedGivenEq ct lty rty
        go (new:acc) ltys rtys
    go _   _          _          = error "impossible!"

-- | Split an application of injective tycon into the tycon and its
-- arguments that have fundeps

splitInjTyFamApp_maybe :: TcType -> Maybe (TyCon, [TcType])
splitInjTyFamApp_maybe t = do
    (tc, args) <- Type.splitTyConApp_maybe t
    case TyCon.tyConInjectivityInfo tc of
        TyCon.NotInjective -> Nothing
        TyCon.Injective is -> Just (tc, [ arg | (i, arg) <- zip is args, i ])

-- | Emit a new Given equality constraint implied by another Given
-- equality constraint

impliedGivenEq :: Ct -> TcType -> TcType -> M Ct
impliedGivenEq ct lty rty = do
    let
      new_co :: Coercion
      new_co = Coercion.mkUnivCo
        (TyCoRep.PluginProv "inj-tyfam-plugin")
        TyCon.Nominal
        lty
        rty

    -- TODO how to incorporate @ctEvId ct@? (see #15248 on GitLab ghc)
    new_ev <- TcPluginM.newGiven
      (TcRnTypes.bumpCtLocDepth (TcRnTypes.ctLoc ct))  -- TODO don't bump?
      (Type.mkPrimEqPredRole TyCon.Nominal lty rty)
      (CoreSyn.Coercion new_co)

    pure $ TcRnTypes.mkNonCanonical new_ev

-----

-- | See 'mkFskEnv'

type FskEnv = VarEnv (TyCon, [Xi])

-- | An incremental map from the @FunEq@s
--
-- NOTE not necessarily idempotent

mkFskEnv :: [Ct] -> FskEnv
mkFskEnv cts =
    VarEnv.mkVarEnv
    [ (cc_fsk, (cc_fun, cc_tyargs))
    | TcRnTypes.CFunEqCan{..} <- cts
    ]

unfsk :: FskEnv -> TcType -> TcType
unfsk fskEnv t = fromMaybe t $ do
    v <- Type.getTyVar_maybe t
    uncurry Type.mkTyConApp <$> VarEnv.lookupVarEnv fskEnv v
