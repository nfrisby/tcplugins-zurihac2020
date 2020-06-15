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
import           TcPluginM (TcPluginM)
import qualified TcRnMonad
import qualified TcRnTypes
import           TcRnTypes (Ct, TcPluginResult, Xi)
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
-- <https://downloads.haskell.org/ghc/8.10.1/docs/html/users_guide/glasgow_exts.html#injective-type-families>
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

-- | The no-op

skip :: TcPluginM TcPluginResult
skip = pure $ TcRnTypes.TcPluginOk [] []

-- | Like @fmap mconcat . sequence@ but short-circuits on
-- 'TcRnTypes.TcPluginContradiction'

mapMR :: (a -> TcPluginM TcPluginResult) -> [a] -> TcPluginM TcPluginResult
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
-- @F@ is a type family with an injectivity annotation, we replace the
-- constraint by new refined constraints.
--
-- The new constraints are @ai ~ bi@ for each argument position @i@
-- that @F@ declares to be injective and where @ai@ and @bi@ are not
-- manifestly equal. This addresses @#10833@. Also, if any @ai@ and
-- @bi@ are manifestly apart, we report a contradiction.
--
-- We also include an additional new constraint @F a1 a2 ... an ~ F c1
-- c2 ... cn@ for which @ci@ is @ai@ if we are also emitting the new
-- constraint @ai ~ bi@ and @bi@ otherwise. We need this new
-- constraint, since some of @F@'s arguments might not be annotated as
-- injective, and so we'd be losing information if we discarded the
-- original equality without adding this new one. We manually update
-- that constraint in case our new point-wise equalities are somehow
-- exotic in such a way that prevents GHC itself from using them to
-- rewrite the original constraint. Else we risk emitting new
-- constraints forever.
--
-- Note: we harmlessly but wastefully emit @F a1 .. an ~ F a1 .. an@
-- when @F@ is injective in all of its arguments; GHC will immediately
-- discharge that spurious constraint, so this doesn't lead to
-- divergence.

simplifyG :: FskEnv -> Ct -> TcPluginM TcPluginResult
simplifyG fskEnv ct =
    case predTree of
      Type.ClassPred{}  -> skip   -- TODO don't need to unpack SCs, right?
      Type.IrredPred{}  -> skip   -- TODO this is unreachable, right?
      Type.ForAllPred{} -> skip   -- TODO this is unreachable, right?
      Type.EqPred eqRel lhs rhs -> case eqRel of
          Type.ReprEq -> skip   -- TODO anything useful to do here?
          Type.NomEq  -> case mkArgInfos fskEnv lhs rhs of
              Nothing         -> skip   -- not the constraint we're looking for
              Just (tc, args) -> do
                  mbChanges <- extractArgEqs [] [] args
                  case mbChanges of
                    Nothing -> pure $ TcRnTypes.TcPluginContradiction [ct]
                    Just x  -> updateOriginalCt lhs tc x
  where
    predTree :: Type.PredTree
    predTree = Type.classifyPredType (TcRnTypes.ctPred ct)

    -- 'Nothing' means contradiction
    -- 'Just' means (updated @rhsArg@s, new constraints)
    extractArgEqs ::
        [Xi] -> [Ct] -> [ArgInfo] -> TcPluginM (Maybe ([Xi], [Ct]))
    extractArgEqs accRhsArgs accNews = \case
        MkArgInfo{..} : args

            -- nothing to learn at this argument position
            | not isInjArg || Type.eqType lhsArg rhsArg ->
            extractArgEqs (rhsArg : accRhsArgs) accNews args

            -- contradiction!
            | Unify.typesCantMatch [(lhsArg, rhsArg)] ->
            pure Nothing

            -- emit new constraint and update the RHS argument
            | otherwise -> do
            new <- impliedGivenEq ct lhsArg rhsArg
            extractArgEqs (lhsArg : accRhsArgs) (new : accNews) args

        [] -> pure $ Just (reverse accRhsArgs, accNews)

    -- replace the equality with one with updated RHS arguments
    updateOriginalCt ::
        TcType -> TyCon -> ([Xi], [Ct]) -> TcPluginM TcPluginResult
    updateOriginalCt lhs tc (rhsArgs', news)
        | null news = skip   -- we made no changes
        | otherwise = do
        new <- impliedGivenEq ct lhs rhs'
        pure $ TcRnTypes.TcPluginOk [(ev, ct)] (new:news)
      where
        rhs' :: TcType
        rhs' = Type.mkTyConApp tc rhsArgs'

        ev :: EvTerm
        ev = TcRnTypes.ctEvTerm $ TcRnTypes.cc_ev ct

-- | Emit a new Given equality constraint implied by another Given
-- equality constraint

impliedGivenEq :: Ct -> TcType -> TcType -> TcPluginM Ct
impliedGivenEq ct lhs rhs = do
    let
      new_co :: Coercion
      new_co = Coercion.mkUnivCo
        (TyCoRep.PluginProv "inj-tyfam-plugin") TyCon.Nominal lhs rhs

    -- TODO how to incorporate @ctEvId ct@? (see #15248 on GitLab ghc)
    new_ev <- TcPluginM.newGiven
      (TcRnTypes.bumpCtLocDepth (TcRnTypes.ctLoc ct))  -- TODO don't bump?
      (Type.mkPrimEqPredRole TyCon.Nominal lhs rhs)
      (CoreSyn.Coercion new_co)

    pure $ TcRnTypes.mkNonCanonical new_ev

-----

-- | See 'mkFskEnv'

type FskEnv = VarEnv (TyCon, [Xi])

-- | A map created from the @FunEq@s
--
-- NOTE not necessarily idempotent

mkFskEnv :: [Ct] -> FskEnv
mkFskEnv cts =
    VarEnv.mkVarEnv
    [ (cc_fsk, (cc_fun, cc_tyargs))
    | TcRnTypes.CFunEqCan{..} <- cts
    ]

-- | Substitute via the 'FskEnv' once

unfsk :: FskEnv -> TcType -> TcType
unfsk fskEnv t = fromMaybe t $ do
    v <- Type.getTyVar_maybe t
    uncurry Type.mkTyConApp <$> VarEnv.lookupVarEnv fskEnv v

-----

-- | A pair of arguments involved in an actionable equality constraint

data ArgInfo = MkArgInfo
    {
      isInjArg :: Bool
      -- ^ whether this argument is included in the family's
      -- injectivity annotation
    ,
      lhsArg :: Xi
      -- ^ the LHS argument at this position
    ,
      rhsArg :: Xi
      -- ^ the RHS argument at this position
    }

-- | See 'ArgInfo'

mkArgInfos :: FskEnv -> TcType -> TcType -> Maybe (TyCon, [ArgInfo])
mkArgInfos fskEnv = \lhs rhs -> do
    let
      prj :: TcType -> Maybe (TyCon, [Xi])
      prj = Type.splitTyConApp_maybe . unfsk fskEnv

    (lhsTyCon, lhsArgs) <- prj lhs
    (rhsTyCon, rhsArgs) <- prj rhs

    guard $ lhsTyCon == rhsTyCon

    case TyCon.tyConInjectivityInfo lhsTyCon of
      TyCon.NotInjective -> Nothing
      TyCon.Injective is ->
          -- INVARIANT: @True == or is@
          if n /= length lhsArgs || n /= length rhsArgs
          then error "impossible!"   -- GHC should have caught this already
          else
            Just (lhsTyCon, zipWith3 MkArgInfo is lhsArgs rhsArgs)
        where
          n = length is
