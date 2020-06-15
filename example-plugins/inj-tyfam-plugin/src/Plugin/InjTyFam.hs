{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Plugin.InjTyFam (
  plugin,
  tcPlugin,
  ) where

-- base
import           Control.Monad (guard)
import           Data.Maybe (fromMaybe)

-- ghc
import qualified GHC.Core
  as Core
    ( Expr(..) )
import qualified GHC.Core.Coercion
  as Core
    ( Coercion )
import qualified GHC.Core.Coercion
  as Core.Coercion
    ( mkPrimEqPredRole, mkUnivCo )
import           GHC.Core.Predicate
    ( Pred(..), EqRel(..) )
import qualified GHC.Core.Predicate
  as Predicate
    ( classifyPredType )
import qualified GHC.Core.TyCo.Rep
  as Core.TyCo.Rep
    ( UnivCoProvenance(..) )
import qualified GHC.Core.Type
  as Core.Type
    ( eqType, getTyVar_maybe, mkTyConApp, splitTyConApp_maybe )
import qualified GHC.Plugins
  as Plugins
    ( Plugin(..), defaultPlugin, purePlugin )
import           GHC.Core.TyCon
    ( TyCon(..) )
import qualified GHC.Core.TyCon
  as Core.TyCon
    ( Injectivity(..), tyConInjectivityInfo )
import qualified GHC.Core.Unify
  as Unify
    ( typesCantMatch )
import qualified GHC.Tc.Plugin
  as Tc.Plugin
    ( newGiven )
import GHC.Tc.Types
    ( TcPlugin(..), TcPluginM, TcPluginResult(..) )
import           GHC.Tc.Types.Constraint
    ( Ct(..), Xi )
import qualified GHC.Tc.Types.Constraint
  as Constraint
    ( Ct(..), ctPred, ctLoc, ctEvTerm
    , bumpCtLocDepth, mkNonCanonical
    )
import qualified GHC.Tc.Types.Evidence
  as TcEv
    ( EvTerm, Role(..) )
import           GHC.Tc.Utils.TcType
    ( TcType )
import           GHC.Types.Var.Env
    ( VarEnv, lookupVarEnv, mkVarEnv )

--------------------------------------------------------------------------------

-- | Just 'tcPlugin', with 'purePlugin' set

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

tcPlugin :: TcPlugin
tcPlugin = TcPlugin
    {
      tcPluginInit = pure ()
    ,
      tcPluginSolve = \() gs ds ws -> do
        let
          fskEnv :: FskEnv
          fskEnv = mkFskEnv gs

        if null ds && null ws
          then mapMR (simplifyG fskEnv) gs   -- we handle G-mode
          else skip   -- GHC already handles W-mode
    ,
      tcPluginStop = \() -> pure ()
    }

-----

-- | The no-op

skip :: TcPluginM TcPluginResult
skip = pure $ TcPluginOk [] []

-- | Like @fmap mconcat . sequence@ but short-circuits on
-- 'TcPluginContradiction'

mapMR :: (a -> TcPluginM TcPluginResult) -> [a] -> TcPluginM TcPluginResult
mapMR f = go [] []
  where
    go xs ys = \case
        []   -> pure $ TcPluginOk xs ys
        a:as -> do
            r <- f a
            case r of
              TcPluginContradiction{} -> pure r
              TcPluginOk x y          -> go (x <> xs) (y <> ys) as

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
    case ctPredicate of
      ClassPred{}  -> skip   -- TODO don't need to unpack SCs, right?
      IrredPred{}  -> skip   -- TODO this is unreachable, right?
      ForAllPred{} -> skip   -- TODO this is unreachable, right?
      EqPred eqRel lhs rhs -> case eqRel of
          ReprEq -> skip   -- TODO anything useful to do here?
          NomEq  -> case mkArgInfos fskEnv lhs rhs of
              Nothing         -> skip   -- not the constraint we're looking for
              Just (tc, args) -> do
                  mbChanges <- extractArgEqs [] [] args
                  case mbChanges of
                    Nothing -> pure $ TcPluginContradiction [ct]
                    Just x  -> updateOriginalCt lhs tc x
  where
    ctPredicate :: Pred
    ctPredicate = Predicate.classifyPredType (Constraint.ctPred ct)

    -- 'Nothing' means contradiction
    -- 'Just' means (updated @rhsArg@s, new constraints)
    extractArgEqs ::
        [Xi] -> [Ct] -> [ArgInfo] -> TcPluginM (Maybe ([Xi], [Ct]))
    extractArgEqs accRhsArgs accNews = \case
        MkArgInfo{..} : args

            -- nothing to learn at this argument position
            | not isInjArg || Core.Type.eqType lhsArg rhsArg ->
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
        pure $ TcPluginOk [(ev, ct)] (new:news)
      where
        rhs' :: TcType
        rhs' = Core.Type.mkTyConApp tc rhsArgs'

        ev :: TcEv.EvTerm
        ev = Constraint.ctEvTerm $ Constraint.cc_ev ct

-- | Emit a new Given equality constraint implied by another Given
-- equality constraint

impliedGivenEq :: Ct -> TcType -> TcType -> TcPluginM Ct
impliedGivenEq ct lhs rhs = do
    let
      new_co :: Core.Coercion
      new_co = Core.Coercion.mkUnivCo
        (Core.TyCo.Rep.PluginProv "inj-tyfam-plugin") TcEv.Nominal lhs rhs

    -- TODO how to incorporate @ctEvId ct@? (see #15248 on GitLab ghc)
    new_ev <- Tc.Plugin.newGiven
      (Constraint.bumpCtLocDepth (Constraint.ctLoc ct))  -- TODO don't bump?
      (Core.Coercion.mkPrimEqPredRole TcEv.Nominal lhs rhs)
      (Core.Coercion new_co)

    pure $ Constraint.mkNonCanonical new_ev

-----

-- | See 'mkFskEnv'

type FskEnv = VarEnv (TyCon, [Xi])

-- | A map created from the @FunEq@s
--
-- NOTE not necessarily idempotent

mkFskEnv :: [Ct] -> FskEnv
mkFskEnv cts =
    mkVarEnv
    [ (cc_fsk, (cc_fun, cc_tyargs))
    | Constraint.CFunEqCan{..} <- cts
    ]

-- | Substitute via the 'FskEnv' once

unfsk :: FskEnv -> TcType -> TcType
unfsk fskEnv t = fromMaybe t $ do
    v <- Core.Type.getTyVar_maybe t
    uncurry Core.Type.mkTyConApp <$> lookupVarEnv fskEnv v

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
      prj = Core.Type.splitTyConApp_maybe . unfsk fskEnv

    (lhsTyCon, lhsArgs) <- prj lhs
    (rhsTyCon, rhsArgs) <- prj rhs

    guard $ lhsTyCon == rhsTyCon

    case Core.TyCon.tyConInjectivityInfo lhsTyCon of
      Core.TyCon.NotInjective -> Nothing
      Core.TyCon.Injective is ->
          -- INVARIANT: @True == or is@
          if n /= length lhsArgs || n /= length rhsArgs
          then error "impossible!"   -- GHC should have caught this already
          else
            Just (lhsTyCon, zipWith3 MkArgInfo is lhsArgs rhsArgs)
        where
          n = length is
