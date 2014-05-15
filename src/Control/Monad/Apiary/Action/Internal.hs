{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE Rank2Types #-}

module Control.Monad.Apiary.Action.Internal where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Base
import Control.Monad.Reader
import Control.Monad.Trans.Control
import Network.Wai
import Network.Mime
import Data.Default.Class
import Data.Monoid
import Network.HTTP.Types
import Blaze.ByteString.Builder
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import qualified Data.Text as T
import Data.Conduit
import qualified Control.Monad.Logger as Logger

data ApiaryConfig = ApiaryConfig
    { -- | call when no handler matched.
      notFound      :: Application
      -- | used unless call 'status' function.
    , defaultStatus :: Status
      -- | initial headers.
    , defaultHeader :: ResponseHeaders
      -- | used by 'Control.Monad.Apiary.Filter.root' filter.
    , rootPattern   :: [S.ByteString]
    , mimeType      :: FilePath -> S.ByteString
    }

instance Default ApiaryConfig where
    def = ApiaryConfig 
        { notFound = \_ -> return $ responseLBS status404 
            [("Content-Type", "text/plain")] "404 Page Notfound."
        , defaultStatus = ok200
        , defaultHeader = []
        , rootPattern   = ["", "/", "/index.html", "/index.htm"]
        , mimeType      = defaultMimeLookup . T.pack
        }

data ActionState 
    = ActionState
        { actionStatus  :: Status
        , actionHeaders :: ResponseHeaders
        , actionBody    :: Body
        }

data Body 
    = File FilePath (Maybe FilePart)
    | Builder Builder
    | LBS L.ByteString
    | SRC (Source IO (Flush Builder))

actionStateToResponse :: ActionState -> Response
actionStateToResponse as = case actionBody as of
    File f p  -> responseFile st hd f p
    Builder b -> responseBuilder st hd b
    LBS l     -> responseLBS st hd l
    SRC    s  -> responseSource st hd s
  where
    st = actionStatus  as
    hd = actionHeaders as

newtype ActionT m a = ActionT { unActionT :: forall b. 
    ApiaryConfig
    -> Request
    -> ActionState
    -> (a -> ActionState -> m (Maybe b))
    -> m (Maybe b)
    }

instance Functor (ActionT m) where
    fmap f m = ActionT $ \conf req st cont ->
        unActionT m conf req st (\a s' -> s' `seq` cont (f a) s')

instance Applicative (ActionT m) where
    pure x = ActionT $ \_ _ st cont -> cont x st
    mf <*> ma = ActionT $ \conf req st cont ->
        unActionT mf conf req st  $ \f st'  ->
        unActionT ma conf req st' $ \a st'' ->
        st' `seq` st'' `seq` cont (f a) st''

instance Monad m => Monad (ActionT m) where
    return x = ActionT $ \_ _ st cont -> cont x st
    m >>= k  = ActionT $ \conf req st cont ->
        unActionT m conf req st $ \a st' ->
        st' `seq` unActionT (k a) conf req st' cont
    fail _ = ActionT $ \_ _ _ _ -> return Nothing

instance MonadIO m => MonadIO (ActionT m) where
    liftIO m = ActionT $ \_ _ st cont ->
        liftIO m >>= \a -> cont a st

instance MonadTrans ActionT where
    lift m = ActionT $ \_ _ st cont ->
        m >>= \a -> cont a st

runActionT :: Monad m => ActionT m a
           -> ApiaryConfig -> Request -> ActionState
           -> m (Maybe (a, ActionState))
runActionT m conf req st = unActionT m conf req st $ \a st' ->
    st' `seq` return (Just (a, st'))

actionT :: Monad m 
        => (ApiaryConfig -> Request -> ActionState -> m (Maybe (a, ActionState)))
        -> ActionT m a
actionT f = ActionT $ \conf req st cont -> f conf req st >>= \case
    Nothing      -> return Nothing
    Just (a,st') -> st' `seq` cont a st'

hoistActionT :: (Monad m, Monad n)
             => (forall b. m b -> n b) -> ActionT m a -> ActionT n a
hoistActionT run m = actionT $ \c r s -> run (runActionT m c r s)

execActionT :: ApiaryConfig -> ActionT IO () -> Application
execActionT config m request = runActionT m config request resp >>= \case
        Nothing    -> notFound config request
        Just (_,r) -> return $ actionStateToResponse r
  where
    resp = ActionState (defaultStatus config) (defaultHeader config) (LBS "")

instance (Monad m, Functor m) => Alternative (ActionT m) where
    empty = mzero
    (<|>) = mplus

instance Monad m => MonadPlus (ActionT m) where
    mzero = actionT $ \_ _ _ -> return Nothing
    mplus m n = actionT $ \c r s -> runActionT m c r s >>= \case
        Just a  -> return $ Just a
        Nothing -> runActionT n c r s

instance Monad m => Monoid (ActionT m ()) where
    mempty  = mzero
    mappend = mplus

instance MonadBase b m => MonadBase b (ActionT m) where
    liftBase = liftBaseDefault

instance MonadTransControl ActionT where
    newtype StT ActionT a = StActionT { unStActionT :: Maybe (a, ActionState) }
    liftWith f = actionT $ \c r s -> 
        liftM (\a -> Just (a,s)) (f $ \t -> liftM StActionT $ runActionT t c r s)
    restoreT m = actionT $ \_ _ _ -> liftM unStActionT m

instance MonadBaseControl b m => MonadBaseControl b (ActionT m) where
    newtype StM (ActionT m) a = StMT { unStMT :: ComposeSt ActionT m a }
    liftBaseWith = defaultLiftBaseWith StMT
    restoreM     = defaultRestoreM unStMT

instance MonadReader r m => MonadReader r (ActionT m) where
    ask     = lift ask
    local f = hoistActionT $ local f

instance Logger.MonadLogger m => Logger.MonadLogger (ActionT m) where
    monadLoggerLog loc src lv msg = lift $ Logger.monadLoggerLog loc src lv msg

getRequest :: Monad m => ActionT m Request
getRequest = ActionT $ \_ r s c -> c r s

getConfig :: Monad m => ActionT m ApiaryConfig
getConfig = ActionT $ \c _ s cont -> cont c s

modifyState :: Monad m => (ActionState -> ActionState) -> ActionT m ()
modifyState f = ActionT $ \_ _ s c -> c () (f s)

-- | when request header is not found, mzero(pass next handler).
getRequestHeader' :: Monad m => HeaderName -> ActionT m S.ByteString
getRequestHeader' h = getRequestHeader h >>= maybe mzero return

getRequestHeader :: Monad m => HeaderName -> ActionT m (Maybe S.ByteString)
getRequestHeader h = (lookup h . requestHeaders) `liftM` getRequest

-- | when query parameter is not found, mzero(pass next handler).
getQuery' :: Monad m => S.ByteString -> ActionT m (Maybe S.ByteString)
getQuery' q = getQuery q >>= maybe mzero return

getQuery :: Monad m => S.ByteString -> ActionT m (Maybe (Maybe S.ByteString))
getQuery q = (lookup q . queryString) `liftM` getRequest

status :: Monad m => Status -> ActionT m ()
status st = modifyState (\s -> s { actionStatus = st } )

modifyHeader :: Monad m => (ResponseHeaders -> ResponseHeaders) -> ActionT m ()
modifyHeader f = modifyState (\s -> s {actionHeaders = f $ actionHeaders s } )

addHeader :: Monad m => HeaderName -> S.ByteString -> ActionT m ()
addHeader h v = modifyHeader ((h,v):)

setHeaders :: Monad m => ResponseHeaders -> ActionT m ()
setHeaders hs = modifyHeader (const hs)

contentType :: Monad m => S.ByteString -> ActionT m ()
contentType c = modifyHeader
    (\h -> ("Content-Type", c) : filter (("Content-Type" /=) . fst) h)

-- | set body to file content and detect Content-Type by extension.
file :: Monad m => FilePath -> Maybe FilePart -> ActionT m ()
file f p = do
    mime <- mimeType <$> getConfig
    contentType (mime f)
    file' f p

file' :: Monad m => FilePath -> Maybe FilePart -> ActionT m ()
file' f p = modifyState (\s -> s { actionBody = File f p } )

builder :: Monad m => Builder -> ActionT m ()
builder b = modifyState (\s -> s { actionBody = Builder b } )

lbs :: Monad m => L.ByteString -> ActionT m ()
lbs l = modifyState (\s -> s { actionBody = LBS l } )

source :: Monad m => Source IO (Flush Builder) -> ActionT m ()
source src = modifyState (\s -> s { actionBody = SRC src } )