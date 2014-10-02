{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Web.Apiary.Database.Persist
    ( Persist
    -- * initializer
    , Migrator(..), With
    , initPersist,     initPersistNoLog
    , initPersistPool, initPersistPoolNoLog
    -- ** low level
    , initPersist', initPersistPool'
    -- * query
    , runSql
    -- * filter
    , sql
    ) where

import Data.Pool
import Control.Monad
import Control.Monad.Logger
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Control

import Database.Persist.Sql

import Web.Apiary
import Web.Apiary.Logger
import qualified Data.Apiary.Dict as Dict
import Data.Apiary.Compat
import Data.Apiary.Document
import Data.Apiary.Extension
import Data.Apiary.Extension.Internal
import Control.Monad.Apiary.Filter.Internal

data Migrator
    = Logging Migration
    | Silent  Migration
    | Unsafe  Migration
    | NoMigrate

data Persist
    = PersistPool ConnectionPool
    | PersistConn Connection

type With c m = forall a. (c -> m a) -> m a

initPersist' :: (MonadIO n, MonadBaseControl IO n) 
             => (forall a. m a -> n a) -> (forall a. Extensions exts -> n a -> m a)
             -> With Connection n -> Migrator -> Initializer m exts (Persist ': exts)
initPersist' wrap run with migr = Initializer $ \es m -> run es $
    with $ \conn -> do
        doMigration migr conn
        wrap $ m (addExtension (PersistConn conn) es)

-- | construct persist extension initializer with no connection pool.
--
-- example: 
--
-- @
-- initPersist (withSqliteConn "db.sqlite") migrateAll
-- @
initPersist :: (MonadIO m, MonadBaseControl IO m) 
            => With Connection (LogWrapper exts m) -> Migration
            -> Initializer m exts (Persist ': exts)
initPersist with = initPersist' logWrapper runLogWrapper with . Logging

initPersistNoLog :: (MonadIO m, MonadBaseControl IO m) 
                 => With Connection (NoLoggingT m)
                 -> Migration -> Initializer m es (Persist ': es)
initPersistNoLog with = initPersist' NoLoggingT (const runNoLoggingT) with . Silent

initPersistPool' :: (MonadIO n, MonadBaseControl IO n)
                 => (forall a. m a -> n a) -> (forall a. Extensions exts -> n a -> m a)
                 -> With ConnectionPool n -> Migrator -> Initializer m exts (Persist ': exts)
initPersistPool' wrap run with migr = Initializer $ \es m -> run es $
    with $ \pool -> do
        withResource pool $ doMigration migr
        wrap $ m (addExtension (PersistPool pool) es)

initPersistPool :: (MonadIO m, MonadBaseControl IO m)
                => With ConnectionPool (LogWrapper exts m) -> Migration
                -> Initializer m exts (Persist ': exts)
initPersistPool with = initPersistPool' logWrapper runLogWrapper with . Logging

initPersistPoolNoLog :: (MonadIO m, MonadBaseControl IO m)
                     => With ConnectionPool (NoLoggingT m)
                     -> Migration -> Initializer m es (Persist ': es)
initPersistPoolNoLog with = initPersistPool' NoLoggingT (const runNoLoggingT) with . Silent

doMigration :: (MonadIO m, MonadBaseControl IO m) => Migrator -> Connection -> m ()
doMigration migr conn = case migr of
    Logging m -> runReaderT (runMigration m) conn
    Silent  m -> runReaderT (void (runMigrationSilent m)) conn
    Unsafe  m -> runReaderT (runMigrationUnsafe m) conn
    NoMigrate -> return ()

-- | execute sql in action.
runSql :: (Has Persist exts, MonadBaseControl IO m)
       => SqlPersistT (ActionT exts prms m) a -> ActionT exts prms m a
runSql a = getExt (Proxy :: Proxy Persist) >>= \case
    PersistPool p -> runSqlPool a p
    PersistConn c -> runSqlConn a c

-- | filter by sql query. since 0.9.0.0.
sql :: (Has Persist exts, MonadBaseControl IO actM, NotMember k prms)
    => Maybe Html -- ^ documentation.
    -> proxy k
    -> SqlPersistT (ActionT exts prms actM) a
    -> (a -> Maybe b) -- ^ result check function. Nothing: fail filter, Just a: success filter and add parameter.
    -> ApiaryT exts (k := b ': prms) actM m () -> ApiaryT exts prms actM m ()
sql doc k q p = focus (maybe id DocPrecondition doc) $ do
    fmap p (runSql q) >>= \case
        Nothing -> mzero
        Just a  -> Dict.insert k a `fmap` getParams
