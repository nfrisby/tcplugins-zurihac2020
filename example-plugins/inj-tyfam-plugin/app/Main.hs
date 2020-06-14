{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -fplugin=Plugin.InjTyFam #-}
{-# OPTIONS_GHC -dcore-lint #-}

module Main (main) where

import Prelim

main :: IO ()
main = pure ()

_id :: (Id a ~ Id b) => a -> b
_id x = x

_both :: (Both a x ~ Both b y) => (a, x) -> (b, y)
_both x = x

_rite :: (Rite a x ~ Rite b y) => (a, b, x) -> (a, b, y)
_rite x = x

-- | This fails to type-check if the plugin only emits the @ai ~ bi@
-- constraints without the updated @F a1 .. an ~ F c1 .. cn@.
_rite2 :: forall a b x y. (Rite a x ~ Rite b y) => (a, b, x) -> (a, b, y)
_rite2 x = (() :: (Rite a y ~ Rite b x) => ()) `seq` x

{-

-- TODO figure out a robust yet lightweight mechanism to test for
-- expected type errors

    • Could not deduce: a ~ b
      from the context: Rite a x ~ Rite b y
        bound by the type signature for:
                   _bad :: forall a x b y.
                           (Rite a x ~ Rite b y) =>
                           (a, x) -> (b, y)

_bad :: (Rite a x ~ Rite b y) => (a, x) -> (b, y)
_bad x = x

-}
