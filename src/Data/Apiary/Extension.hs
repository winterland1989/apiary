{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ConstraintKinds #-}

module Data.Apiary.Extension
    ( Has(getExtension)
    , MonadHas(..)
    , Extension(..)
    , Extensions
    , noExtension
    -- * initializer constructor
    , Initializer,  initializer
    , Initializer', initializer'
    , initializerBracket
    , initializerBracket'

    -- * combine initializer
    , (+>)
    ) where

import Data.Apiary.Extension.Internal

class MonadHas e m where
    getExt :: proxy e -> m e

type Initializer' m a = forall i. Initializer m i (a ': i)

addExtension :: Extension e => e -> Extensions es -> Extensions (e ': es)
addExtension = AddExtension

initializer :: (Extension e, Monad m) => (Extensions es -> m e) -> Initializer m es (e ': es)
initializer m = Initializer $ \es n -> do
    e <- m es
    n (addExtension e es)

initializer' :: (Extension e, Monad m) => m e -> Initializer' m e
initializer' m = initializer (const m)

initializerBracket :: Extension e => (forall a. Extensions es -> (e -> m a) -> m a) -> Initializer m es (e ': es)
initializerBracket b = Initializer $ \es n ->
    b es $ \e -> n (addExtension e es)

initializerBracket' :: Extension e => (forall a. (e -> m a) -> m a) -> Initializer m es (e ': es)
initializerBracket' m = initializerBracket (const m)

-- | combine two Initializer. since 0.16.0.
(+>) :: Monad m => Initializer m i x -> Initializer m x o -> Initializer m i o
Initializer a +> Initializer b = Initializer $ \e m -> a e (\e' -> b e' m)

noExtension :: Monad m => Initializer m i i
noExtension = Initializer $ \es n -> n es
