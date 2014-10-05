{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE StandaloneDeriving #-}

-- | type sefe dictionaly.
module Data.Apiary.Dict
    ( Dict
    , empty
    , insert
    , Member(get)
    , key

    -- * types
    , Elem((:=))
    , NotMember
    , Member'
    , Members
    ) where

import Data.Apiary.Compat

import Language.Haskell.TH
import Language.Haskell.TH.Quote
import GHC.Exts

-- | (kind) Dict element.
data Elem = forall a. Symbol := a

data Dict (ks :: [Elem]) where
    Empty :: Dict '[]
    Insert :: proxy (k :: Symbol) -> v -> Dict ks -> Dict (k := v ': ks)


class Member (k :: Symbol) (v :: *) (kvs :: [Elem]) | k kvs -> v where

    -- | get value of key.
    --
    -- > ghci> get (SProxy :: SProxy "bar") $ insert (SProxy :: SProxy "bar") (0.5 :: Double) $ insert (SProxy :: SProxy "foo") (12 :: Int) empty
    -- > 0.5
    --
    -- > ghci> get (SProxy :: SProxy "foo") $ insert (SProxy :: SProxy "bar") (0.5 :: Double) $ insert (SProxy :: SProxy "foo") (12 :: Int) empty
    -- > 12
    --
    -- ghc raise compile error when key is not exists.
    --
    -- > ghci> get (SProxy :: SProxy "baz") $ insert (SProxy :: SProxy "bar") (0.5 :: Double) $ insert (SProxy :: SProxy "foo") (12 :: Int) empty
    -- > <interactive>:15:1:
    -- >     No instance for (Member "baz" a0 '[]) arising from a use of ‘it’
    -- >     In the first argument of ‘print’, namely ‘it’
    -- >     In a stmt of an interactive GHCi command: print it

    get :: proxy k -> Dict kvs -> v

instance Member k v (k := v ': kvs) where
    get _ (Insert _ v _) = v

instance Member k v kvs => Member k v (k' := v' ': kvs) where
    get p (Insert _ _ d) = get p d

-- | type family version Member for NotMember constraint.
#if __GLASGOW_HASKELL__ && __GLASGOW_HASKELL__ >= 708
type family Member' (k::Symbol) (kvs :: [Elem]) :: Bool where
    Member' k  '[] = False
    Member' k  (k := v ': kvs) = True
    Member' k' (k := v ': kvs) = Member' k' kvs
#else
type family   Member' (k::Symbol) (kvs :: [Elem]) :: Bool
type instance Member' k kvs = False
#endif

type NotMember k kvs = Member' k kvs ~ False

-- | type family to constraint multi kvs.
--
-- > Members ["foo" := Int, "bar" := Double] prms == (Member "foo" Int prms, Member "bar" Double prms)
--
type family Members (kvs :: [Elem]) (prms :: [Elem]) :: Constraint
type instance Members '[] prms = ()
type instance Members (k := v ': kvs) prms = (Member k v prms, Members kvs prms)

-- | empty Dict.
empty :: Dict '[]
empty = Empty

-- | insert element.
-- 
-- > ghci> :t insert (SProxy :: SProxy "foo") (12 :: Int) empty
-- > insert (SProxy :: SProxy "foo") (12 :: Int) empty
-- >   :: Dict '["foo" ':= Int]
-- 
-- > ghci> :t insert (SProxy :: SProxy "bar") (0.5 :: Double) $ insert (SProxy :: SProxy "foo") (12 :: Int) empty
-- > insert (SProxy :: SProxy "bar") (0.5 :: Double) $ insert (SProxy :: SProxy "foo") (12 :: Int) empty
-- >   :: Dict '["bar" ':= Double, "foo" ':= Int]
--
-- ghc raise compile error when insert duplicated key(> ghc-7.8 only).
--
-- > ghci> :t insert (SProxy :: SProxy "foo") (0.5 :: Double) $ insert (SProxy :: SProxy "foo") (12 :: Int) empty
-- > 
-- > <interactive>:1:1:
-- >     Couldn't match type ‘'True’ with ‘'False’
-- >     Expected type: 'False
-- >       Actual type: Member' "foo" '["foo" ':= Int]
-- >     In the expression: insert (SProxy :: SProxy "foo") (0.5 :: Double)
-- >     In the expression:
-- >       insert (SProxy :: SProxy "foo") (0.5 :: Double)
-- >       $ insert (SProxy :: SProxy "foo") (12 :: Int) empty

insert :: NotMember k kvs => proxy k -> v -> Dict kvs -> Dict (k := v ': kvs)
insert = Insert

-- | construct string literal proxy.
--
-- prop> [key|foo|] == (SProxy :: SProxy "foo")
--
key :: QuasiQuoter
key = QuasiQuoter
    { quoteExp  = \s -> [| SProxy :: SProxy $(litT $ strTyLit s) |]
    , quotePat  = error "key qq only exp or type."
    , quoteType = \s -> [t| SProxy $(litT $ strTyLit s) |]
    , quoteDec  = error "key qq only exp or type."
    }
