{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}

module Data.Apiary.Routing.Dict
    ( -- * store
      Store
    , emptyStore
    , type (</)
    , add
    , mkDict

      -- * dictionary
    , Dict
    , emptyDict
    , Member
    , get

      -- * types
    , ShowDict
    , KV(..)

      -- * convenient
    , Members

      -- * pretty print type errors
    , GetResult(..)
    , AddResult(..)
    ) where

import GHC.Exts(Constraint)
import Data.Apiary.Routing.Dict.Internal

-- | type family to constraint multi kvs.
--
-- > Members ["foo" := Int, "bar" := Double] prms == (Member "foo" Int prms, Member "bar" Double prms)
--
type family   Members (kvs :: [KV *]) (prms :: [KV *]) :: Constraint
type instance Members '[] prms = ()
type instance Members (k ':= v ': kvs) prms = (Member k v prms, Members kvs prms)
