{-# LANGUAGE FlexibleContexts #-} -- for ghc-7.6
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

-- | >>> :set -XDataKinds -XPolyKinds -XOverloadedStrings
-- >>> data Proxy s = Proxy
-- >>> import Text.Read(readMaybe)
-- >>> import qualified Data.Text as T
--
-- 1. create path
--
-- >>> data Result = A | B T.Text | C | D Int | E T.Text deriving Show
--
-- >>> let key = Proxy :: Proxy "key"
--
-- >>> let a = root $ exact "foo" $ action Nothing (\_ -> Just A)
-- >>> let b = root $ exact "bar" $ fetch key Just $ action Nothing (\d -> Just . B $ D.get key d)
-- >>> let c = root $ exact "bar" $ any $ action (Just "GET") (\_ -> Just C)
-- >>> let d = root $ exact "bar" $ fetch key (\t -> readMaybe (T.unpack t) :: Maybe Int) $ action Nothing (\d -> Just . D $ D.get key d)
-- >>> let e = root $ exact "foo" $ fetch key Just $ exact "qux" $ action (Just "POST") (\d -> Just . E $ D.get key d)
-- >>> a
-- * /foo
-- >>> b
-- * /bar/:key
-- >>> c
-- GET /bar/**
-- >>> d
-- * /bar/:key
-- >>> e
-- POST /foo/:key/qux
--
-- 2. create router
--
-- >>> let r = e +| d +| a +| b +| c +| empty
--
-- 3. execute router
--
-- >>> let run = execute r
--
-- >>> run "GET" ["foo"]
-- Just A
-- >>> run "GET" ["foo", "bar"]
-- Nothing
-- >>> run "GET" ["bar", "12"]
-- Just (D 12)
-- >>> run "GET" ["bar", "baz"]
-- Just (B "baz")
-- >>> run "GET" ["bar", "baz", "qux"]
-- Just C
-- >>> run "POST" ["bar", "baz", "qux"]
-- Nothing
-- >>> run "POST" ["foo", "bar", "baz"]
-- Nothing
-- >>> run "POST" ["foo", "bar", "qux"]
-- Just (E "bar")

module Data.Apiary.Routing
    ( Method
      -- * Path
    , Path
    , showPath, getMethod
    , root
      -- ** children
    , exact
      -- ** action
    , action
      -- ** get parameter
    , Raw
    , raw
    , fetch
    , any
    , rest

    -- * Router
    , Router
    , empty
    , insert, (+|)

    -- * execute
    , execute

    -- * reexport
    -- | 'Store', '</', 'add'
    --
    -- 'Dict', 'Member', 'get'
    --
    -- 'KV'((':=')), 'Members'
    , module Data.Apiary.Routing.Dict
    ) where

import Prelude hiding(any)
import Control.Monad(MonadPlus(..))

import GHC.TypeLits(KnownSymbol, symbolVal)
import qualified Data.Apiary.Routing.Dict.Internal as D
import qualified Data.Text as T
import qualified Data.HashMap.Strict as H
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as SC

import Data.Apiary.Routing.Dict
    ( Store
    , type (</)
    , add

    , Dict
    , Member
    , get

    , KV(..)

    , Members
    )

type Method = S.ByteString

data Params d m a where
    PCons :: !(D.Store d -> [T.Text] -> m (D.Store d', [T.Text]))
          -> !(Router d' m a)
          -> !(Params d m a) -> Params d m a
    PNil  :: Params d m a

-- | routing path
data Path d m a where
    Exact :: !T.Text -> !(Path d m a) -> Path d m a

    Param :: !String -> (D.Store d -> [T.Text] -> m (D.Store d', [T.Text]))
          -> !(Path d' m a) -> Path d m a

    Action :: !(Maybe Method)
           -> (D.Dict d -> m a) -> Path d m a

-- | root
--
-- @
-- root == id
-- @
root :: Path '[] m a -> Path '[] m a
root = id
{-# INLINABLE root #-}

-- | exact matching path
exact :: T.Text -> Path d m a -> Path d m a
exact = Exact
{-# INLINABLE exact #-}

type Raw m d d'
    = D.Store d -- ^ input dictionary
    -> [T.Text] -- ^ input path information
    -> m (D.Store d', [T.Text]) -- ^ output dictionary and path information

-- | raw get parameter function
--
-- if you want matching exact path, use 'exact' for performance
raw :: String -- ^ pretty print
    -> Raw m d d'
    -> Path d' m a -> Path d m a
raw = Param
{-# INLINABLE raw #-}

-- ^ get one directory as parameter.
fetch :: (MonadPlus m, KnownSymbol k, k D.</ d)
      => proxy k -- ^ dictionary key
      -> (T.Text -> Maybe v) -- ^ reading function
      -> Path (k 'D.:= v ': d) m a -> Path d m a
fetch p f = Param (':' : symbolVal p) go
  where
    go _ [] = mzero
    go d (t:ts) = case f t of
        Nothing -> mzero
        Just v  -> return (D.add p v d, ts)
{-# INLINABLE fetch #-}

-- | drop any pathes
any :: Monad m => Path d m a -> Path d m a
any = Param "**" go
  where
    go d _ = return (d, [])
{-# INLINABLE any #-}

-- | take any pathes as [Text]
rest :: (KnownSymbol k, Monad m, k D.</ d) => proxy k -- ^ dictionary key
     -> Path (k 'D.:= [T.Text] ': d) m a -> Path d m a
rest k = Param (':': symbolVal k ++ "**") go
  where
    go d r = return (D.add k r d, [])
{-# INLINABLE rest #-}

-- | action
action :: Maybe Method -- ^ if Nothing, any method allowed
       -> (D.Dict d -> m a) -- ^ action when route matching
       -> Path d m a
action  = Action
{-# INLINABLE action #-}

instance Show (Path d m a) where
    show p = maybe "*" SC.unpack (getMethod p) ++ ' ': showPath p

-- | show path. since v0.6.0.
showPath :: Path d m a -> String
showPath = go id
  where
    go :: (String -> String) -> Path d m a -> String
    go s (Exact t   ps) = go (s . (++) ('/' : T.unpack t)) ps
    go s (Param l _ ps) = go (s . (++) ('/' : l)) ps
    go s (Action _ _)   = s []

-- | get method. since v0.6.0.
getMethod :: Path d m a -> Maybe Method
getMethod = go
  where
    go :: Path d m a -> Maybe Method
    go (Action m _)   = m
    go (Exact _   ps) = go ps
    go (Param _ _ ps) = go ps

-- | router
data Router d m a where
    Router ::
        { params    :: !(Params d m a)
        , children  :: !(H.HashMap T.Text (Router d m a))
        , methods   :: !(H.HashMap Method (D.Dict d -> m a))
        , anyMethod :: D.Dict d -> m a
        } -> Router d m a

emptyRouter :: MonadPlus m => Router d m a
emptyRouter = Router { params    = PNil
                     , children  = H.empty
                     , methods   = H.empty
                     , anyMethod = const mzero
                     }
{-# INLINABLE emptyRouter #-}

-- | empty router
empty :: MonadPlus m => Router '[] m a
empty = emptyRouter
{-# INLINABLE empty #-}

insert' :: MonadPlus m => Path d m a -> Router d m a -> Router d m a
insert' (Exact p n) r =
    let c = H.lookupDefault emptyRouter p (children r)
    in r { children = H.insert p (insert' n c) (children r) }

insert' (Param _ f n) Router{..} = Router
    { params    = PCons f (insert' n emptyRouter) params
    , children  = children
    , methods   = methods
    , anyMethod = anyMethod
    }

insert' (Action (Just m) n) r =
    let c = case H.lookup m (methods r) of
            Nothing -> \d -> n d
            Just p  -> \d -> p d `mplus` n d
    in r { methods = H.insert m c (methods r) }

insert' (Action Nothing n) r =
    r { anyMethod = \d -> anyMethod r d `mplus` n d }
{-# INLINABLE insert' #-}

-- | insert path to router
insert :: MonadPlus m => Path '[] m a -> Router '[] m a -> Router '[] m a
insert = insert'
{-# INLINABLE insert #-}

-- | infix version of `insert`
(+|) :: MonadPlus m => Path '[] m a -> Router '[] m a -> Router '[] m a
(+|) = insert
{-# INLINABLE (+|) #-}

infixr `insert`
infixr +|

-- | execute router
execute :: MonadPlus m => Router '[] m a -> Method -> [T.Text] -> m a
execute = {-# SCC execute #-} execute' D.emptyStore
{-# INLINABLE execute #-}

execute' :: MonadPlus m => D.Store d -> Router d m a -> Method -> [T.Text] -> m a
execute' d Router{params, methods, anyMethod} m [] = {-# SCC execute' #-} fetching next d m [] params
  where
    next = case H.lookup m methods of
        Nothing -> anyMethod (D.mkDict d)
        Just f  -> f (D.mkDict d)

execute' d Router{params, children} m pps@(p:ps) = {-# SCC execute' #-} case H.lookup p children of
    Nothing -> next
    Just c  -> execute' d c m ps `mplus` next
  where
    next = fetching mzero d m pps params
{-# INLINABLE execute' #-}

fetching :: MonadPlus m => m a -> D.Store d -> Method -> [T.Text] -> Params d m a -> m a
fetching next d m pps = {-# SCC fetching #-} loop
  where
    loop PNil = next
    loop (PCons f r o) =
        do (d', pps') <- f d pps
           execute' d' r m pps'
        `mplus` loop o
{-# INLINABLE fetching #-}
