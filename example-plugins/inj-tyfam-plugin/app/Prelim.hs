{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}

module Prelim (module Prelim) where

type family Id a = r | r -> a

type family Both a b = r | r -> a b

type family Rite a b = r | r -> b
