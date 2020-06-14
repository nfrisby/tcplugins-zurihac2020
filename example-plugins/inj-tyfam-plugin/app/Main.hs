{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -fplugin=Plugin.InjTyFam #-}
{-# OPTIONS_GHC -dcore-lint #-}

module Main (main) where

import Prelim

main :: IO ()
main = test `seq` pure ()

test :: (Id a ~ Id b) => a -> b
test x = x
