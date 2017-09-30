{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE BangPatterns #-} -- for ghc-7.6
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE CPP #-}

#if __GLASGOW_HASKELL__ < 710
{-# LANGUAGE OverlappingInstances #-}
#endif

module Data.Apiary.Routing.Dict.Internal
    ( Dict, Store
    , ShowDict
    , KV(..)
    , emptyStore
    , emptyDict
    , type (</)
    , add
    , Member
    , get
    , mkDict
    , AddResult(..)
    , GetResult(..)
    ) where

import GHC.Exts(Any)

import Data.Proxy
import GHC.TypeLits
import Data.Typeable(typeOf, Typeable, TypeRep)
import Data.List(intercalate)
import Unsafe.Coerce
import qualified Control.Monad.Primitive as P
import qualified Data.Primitive.Array as P
import Control.Monad.ST (ST, runST)

unsafeToAny :: a -> Any
unsafeToAny = unsafeCoerce
{-# INLINABLE unsafeToAny #-}

unsafeFromAny :: Any -> a
unsafeFromAny = unsafeCoerce
{-# INLINABLE unsafeFromAny #-}

-- | (kind) key-value pair
data KV v = Symbol := v

-- | data store to construct dictionary.
--
-- `add` and `mkDict` operation only allowed.
data KVList (kvs :: [KV *]) where
    Cons  :: v -> KVList kvs -> KVList (k ':= v ': kvs)
    Empty :: KVList '[]

instance ShowDict kvs => Show (Store kvs) where
    show d = "Store {" ++
        (intercalate ", " . map (\(k, v, t) -> k ++ " = " ++ v ++ " :: " ++ show t) $ showDict 0 (mkDict d))
        ++ "}"

data Store kvs = Store
    { storeSize :: {-# UNPACK #-} !Int
    , storeBody :: KVList kvs
    }

emptyStore :: Store '[]
emptyStore = Store 0 Empty
{-# INLINABLE emptyStore #-}

emptyDict :: Dict '[]
emptyDict = mkDict emptyStore
{-# INLINABLE emptyDict #-}

-- | (kind) pretty print type error of 'add'.
--
-- @
-- > add (Proxy :: Proxy "a") 12 $ add (Proxy :: Proxy "a") "a" emptyStore
-- Couldn't match type ‘'Dictionary’ with ‘'AlreadyHasKey "a"’
-- @

data AddResult
    = AlreadyHasKey Symbol
    | Dictionary

#if __GLASGOW_HASKELL__ > 707
type family HasKey (k :: Symbol) (kvs :: [KV *]) :: AddResult where
  HasKey k '[] = 'AlreadyHasKey k
  HasKey k (k  ':= v ': kvs) = 'Dictionary
  HasKey k (k' ':= v ': kvs) = HasKey k kvs
#else
type family   HasKey (k :: Symbol) (kvs :: [KV *]) :: AddResult
type instance HasKey k kvs = 'AlreadyHasKey k
#endif

-- | 'not elem key' constraint(ghc >= 7.8)
type k </ v = HasKey k v ~ 'AlreadyHasKey k

-- | O(1) add key value pair to dictionary.
--
-- >>> let a = add (Proxy :: Proxy "foo") (12 :: Int) emptyStore
-- >>> a
-- Store {foo = 12 :: Int}
--
-- >>> add (Proxy :: Proxy "bar") "baz" a
-- Store {bar = "baz" :: [Char], foo = 12 :: Int}
add :: (k </ kvs) => proxy k -> v -> Store kvs -> Store (k ':= v ': kvs)
add _ v (Store l c) = Store (l + 1) (Cons v c)
{-# INLINABLE add #-}

-- | heterogeneous dictionary
--
-- 'get' operation only allowed.
newtype Dict (kvs :: [KV *]) = Dict (P.Array Any)

class ShowDict (kvs :: [KV *]) where
    showDict :: Int -> Dict kvs -> [(String, String, TypeRep)]

instance ShowDict '[] where
    showDict _ _ = []

instance (KnownSymbol k, Typeable v, Show v, ShowDict kvs) => ShowDict (k ':= v ': kvs) where
    showDict i (Dict t) =
        (symbolVal (Proxy :: Proxy k), show (unsafeFromAny $ P.indexArray t i :: v), typeOf (undefined :: v)):
        showDict (i + 1) (unsafeCoerce $ Dict t :: Dict kvs)

instance ShowDict kvs => Show (Dict kvs) where
    show d = "Dict {" ++
        (intercalate ", " . map (\(k, v, t) -> k ++ " = " ++ v ++ " :: " ++ show t) $ showDict 0 d)
        ++ "}"

mkDict' :: forall s kvs. Store kvs -> ST s (Dict kvs)
mkDict' store = do
    ary <- P.newArray (storeSize store) undefined
    go ary
    Dict `fmap` P.unsafeFreezeArray ary
  where
    go :: P.MutableArray (P.PrimState (ST s)) Any -> ST s ()
    go array = loop 0 (storeBody store)
      where
        loop :: Int -> KVList kvs' -> ST s ()
        loop !i (Cons v ss) = do
            P.writeArray array i (unsafeToAny v)
            loop (i + 1) ss
        loop _ Empty = return ()
{-# INLINABLE mkDict' #-}

-- | O(n) convert "Store" to "Dictionary".
mkDict :: Store kvs -> Dict kvs
mkDict store = runST $ mkDict' store
{-# INLINABLE mkDict #-}

-- | O(1) (>= ghc-7.8), O(n) (< ghc-7.8) get key from dictionary
--
-- >>> let d = mkDict $ add (Proxy :: Proxy "foo") 12 $ add (Proxy :: Proxy "bar") "baz" emptyStore
-- >>> get (Proxy :: Proxy "foo") d
-- 12
-- >>> get (Proxy :: Proxy "bar") d
-- "baz"
get :: Member k v kvs => proxy k -> Dict kvs -> v

-- | (kind) pretty print type error of 'get'
--
--  used only >= ghc-7.8
--
-- @
-- > get (Proxy :: Proxy "b") (mkDict $ add (Proxy :: Proxy "a") "a" emptyStore)
-- Couldn't match type ‘'Key "b"’ with ‘'NotInDicrionary i’
-- @
data GetResult
    = NotInDicrionary Nat
    | Key Symbol

#if __GLASGOW_HASKELL__ > 707

type family Ix' (i :: Nat) (k :: Symbol) (kvs :: [KV *]) :: GetResult where
  Ix' i k '[] = 'Key k
  Ix' i k (k  ':= v ': kvs) = 'NotInDicrionary i
  Ix' i k (k' ':= v ': kvs) = Ix' (i + 1) k kvs

type Ix k kvs = Ix' 0 k kvs

type Index = 'NotInDicrionary

getImpl :: forall i proxy k kvs v. (Index i ~ Ix k kvs, KnownNat i) => proxy (k :: Symbol) -> Dict kvs -> v
getImpl _ (Dict d) = unsafeFromAny $ d `P.indexArray` fromIntegral (natVal (Proxy :: Proxy i))
{-# INLINABLE getImpl #-}

class Member (k :: Symbol) (v :: *) (kvs :: [KV *]) | k kvs -> v where
    get' :: proxy k -> Dict kvs -> v

#if __GLASGOW_HASKELL__ >= 710
instance {-# OVERLAPPING #-} Member k v (k ':= v ': kvs) where
#else
instance Member k v (k ':= v ': kvs) where
#endif
    get' = getImpl
    {-# INLINE get' #-}

instance (Member k v kvs, Index i ~ Ix k (k' ':= v' ': kvs), KnownNat i) => Member k v (k' ':= v' ': kvs) where
    get' = getImpl
    {-# INLINE get' #-}

get = get'
#else
class Member (k :: Symbol) (v :: *) (kvs :: [KV *]) | k kvs -> v where
    get' :: Int -> proxy k -> Dict kvs -> v

instance Member k v (k ':= v ': kvs) where
    get' !i _ (Dict d) = unsafeFromAny $ d `P.indexArray` i
    {-# INLINE get' #-}

instance Member k v kvs => Member k v (k' ':= v' ': kvs) where
    get' !i k d = get' (i + 1) k (unsafeCoerce d :: Dict kvs)
    {-# INLINE get' #-}

get = get' 0
#endif
