{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Plugin.InjTyFam (
  plugin,
  tcPlugin,
  ) where

import           Control.Monad (guard)
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
import qualified Unify
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

        if null ds && null ws
          then mapMR (simplifyG fskEnv) gs   -- we handle G-mode
          else skip   -- GHC already handles W-mode
    ,
      TcRnTypes.tcPluginStop = \() -> pure ()
    }

-----

type M = TcPluginM.TcPluginM
type R = TcRnTypes.TcPluginResult

-- | The no-op

skip :: M R
skip = pure $ TcRnTypes.TcPluginOk [] []

-- | A @fmap mconcat . sequence@ that short-circuits on
-- 'TcRnTypes.TcPluginContradiction'

mapMR :: (a -> M R) -> [a] -> M R
mapMR f = go [] []
  where
    go xs ys = \case
        []   -> pure $ TcRnTypes.TcPluginOk xs ys
        a:as -> do
            r <- f a
            case r of
              TcRnTypes.TcPluginContradiction{} -> pure r
              TcRnTypes.TcPluginOk x y          -> go (x <> xs) (y <> ys) as

-----

-- | For a Given constraint @F a1 a2 ... an ~ F b1 b2 ... bn@ where
-- @F@ is a type family with functional dependencies, we replace the
-- constraint by new refined constraints.
--
-- The new constraints are @ai ~ bi@ for each argument position @i@
-- that @F@ declares to be injective and where @ai@ and @bi@ are not
-- manfiestly equal. This addresses @#10833@. Also, if any @ai@ and
-- @bi@ are manifestly apart, we report a contradiction.
--
-- We also include an additional new constraint @F a1 a2 ... an ~ F c1
-- c2 ... cn@ for which @ci@ is @ai@ if we are also emitting the new
-- constraint @ai ~ bi@ and @bi@ otherwise.
--
-- Note that we cannot discard the @F ... ~ F ...@ constraint, since
-- some of its arguments might not have fundeps, and so we'd be losing
-- information. We manually update that constraint in case our new
-- point-wise equalities are somehow exotic in such a way that
-- prevents GHC from using them to rewrite the original constraint.
-- Else we risk emitting new constraints forever.

simplifyG :: FskEnv -> Ct -> M R
simplifyG fskEnv ct =
    case predTree of
      Type.ClassPred{}  -> skip   -- TODO don't need to unpack SCs, right?
      Type.IrredPred{}  -> skip
      Type.ForAllPred{} -> skip   -- TODO this is unreachable, right?
      Type.EqPred eqRel lty rty -> case eqRel of
          Type.ReprEq -> skip   -- TODO anything useful to do here?
          Type.NomEq  -> case splitter fskEnv lty rty of
              Nothing         -> skip
              Just (tc, args) -> do
                  mbChanges <- go [] [] args
                  case mbChanges of
                    Nothing -> pure $ TcRnTypes.TcPluginContradiction [ct]
                    Just x  -> ok lty tc x
  where
    predTree :: Type.PredTree
    predTree = Type.classifyPredType (TcRnTypes.ctPred ct)

    -- 'Nothing' means contradiction
    -- 'Just' means (updated rtys, new constraints)
    go :: [Xi] -> [Ct] -> [Triple] -> M (Maybe ([Xi], [Ct]))
    go acc1 acc2 = \case
        MkTriple lty rty i : args

            -- nothing to learn at this argument position
            | not i || Type.eqType lty rty ->
            go (rty : acc1) acc2 args

            -- contradiction!
            | Unify.typesCantMatch [(lty, rty)] ->
            pure Nothing

            -- emit new constraint and update the RHS argument
            | otherwise -> do
            new <- impliedGivenEq ct lty rty
            go (lty : acc1) (new : acc2) args

        [] -> pure $ Just (reverse acc1, acc2)

    -- replace the equality with one with updated RHS arguments
    ok :: TcType -> TyCon -> ([Xi], [Ct]) -> M R
    ok lty tc (rtys', news)
        | null news = skip
        | otherwise = do
        new <- impliedGivenEq ct lty rty'
        pure $ TcRnTypes.TcPluginOk [(ev, ct)] (new:news)
      where
        rty' :: TcType
        rty' = Type.mkTyConApp tc rtys'

        ev :: EvTerm
        ev = TcRnTypes.ctEvTerm $ TcRnTypes.cc_ev ct

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

-----

-- | A pair of arguments involved in an actionable equality constraint

data Triple = MkTriple Xi Xi Bool
      -- ^ the LHS argument, the RHS argument, and whether this
      -- argument is included in the family's injectivity annotation

-- | See 'Triple'

splitter :: FskEnv -> TcType -> TcType -> Maybe (TyCon, [Triple])
splitter fskEnv = \lty rty -> do
    (ltc, ltys) <- prj lty
    (rtc, rtys) <- prj rty

    guard $ ltc == rtc

    case TyCon.tyConInjectivityInfo ltc of
        TyCon.NotInjective -> Nothing
        TyCon.Injective is ->
            -- INVARIANT: @True == or is@
            Just (ltc, go is ltys rtys)

  where
    prj :: TcType -> Maybe (TyCon, [Xi])
    prj = Type.splitTyConApp_maybe . unfsk fskEnv

    go :: [Bool] -> [Xi] -> [Xi] -> [Triple]
    go []     []         []         = []
    go (i:is) (lty:ltys) (rty:rtys) =
        MkTriple lty rty i : go is ltys rtys
    go _      _          _          = error "impossible"
