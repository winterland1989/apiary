{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Web.Apiary.Database.Redis
    ( -- * initializer
      initRedis
    -- * query
    , runRedis
    , runRedisTx
    ) where

import Control.Monad.IO.Class(MonadIO(..))
import qualified Database.Redis as Redis
import Data.Proxy(Proxy(..))
import Data.Apiary.Extension
    (Has, Initializer, initializer', Extension, MonadExts, getExt)

newtype RedisConn = RedisConn Redis.Connection

instance Extension RedisConn

-- | construct persist extension initializer with no connection pool.
--
-- example:
--
-- @
-- initPersist (withSqliteConn "db.sqlite") migrateAll
-- @
initRedis :: (Monad m)
             => Redis.Connection -> Initializer m exts (RedisConn ': exts)
initRedis conn = initializer' $ return (RedisConn conn)

runRedis :: (MonadExts es m, Has RedisConn es, MonadIO m) => Redis.Redis a -> m a
runRedis r = do
    getExt (Proxy :: Proxy RedisConn) >>= \ (RedisConn c) ->
        liftIO $ Redis.runRedis c r

runRedisTx :: (MonadExts es m, Has RedisConn es, MonadIO m)
           => Redis.RedisTx (Redis.Queued a) -> m (Redis.TxResult a)
runRedisTx r = do
    getExt (Proxy :: Proxy RedisConn) >>= \ (RedisConn c) ->
        liftIO . Redis.runRedis c $ Redis.multiExec r
