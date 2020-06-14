{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}

module Prelim (module Prelim) where

type family Id a = r | r -> a
