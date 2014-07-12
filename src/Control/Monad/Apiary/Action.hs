{-# LANGUAGE CPP #-}

module Control.Monad.Apiary.Action 
    ( ActionT
    , ApiaryConfig(..)
    -- * actions
    , stop, stopWith

    -- ** getter
    , getRequest
    , getHeaders
    , getReqParams
    , getReqFiles

    -- ** setter
    , status
    -- *** response header
    , addHeader, setHeaders, modifyHeader
    , contentType
    -- *** response body
    , file
    , file'
    , builder
    , lbs
    , stream
    , response

    , StreamingBody

    -- ** monolithic action
    -- *** redirect
    , redirect, redirectPermanently, redirectTemporary
    , redirectWith
    -- * Reexport
    , module Data.Default.Class
    , module Network.HTTP.Types.Status
    
    -- * deprecated
    , redirectFound, redirectSeeOther, source
    ) where

import Control.Monad.Apiary.Action.Internal
import Data.Default.Class
import Network.HTTP.Types.Status hiding (mkStatus)

#ifdef WAI3
import Network.Wai
#endif
