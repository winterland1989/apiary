{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}

module Data.Apiary.SProxy(SProxy(..)) where

import GHC.TypeLits(Symbol)

data SProxy (k :: Symbol) = SProxy
