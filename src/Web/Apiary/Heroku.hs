{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE CPP #-}

module Web.Apiary.Heroku
    ( Heroku
    -- * configuration
    , HerokuConfig(..)
    -- * runner
    , runHeroku, runHerokuWith, runHerokuTWith
    -- * extension functions
    , getHerokuEnv, getHerokuEnv'
    ) where

import System.Environment(getEnv)
import System.Process
    ( proc, CreateProcess(..), createProcess
    , StdStream(CreatePipe), waitForProcess)
import System.Exit (ExitCode(ExitSuccess))

import qualified Network.Wai as Wai

import Control.Exception(catch, try, SomeException)
import Control.Arrow(second)
#if !MIN_VERSION_base(4,8,0)
import Control.Applicative((<$>), (<$), Applicative(..))
#endif
import Control.Monad.Trans(MonadIO(..))

import Data.IORef(IORef, newIORef, readIORef, writeIORef)
import Data.Default.Class(Default(def))
import qualified Data.HashMap.Strict as H
import qualified Data.Text    as T
import qualified Data.Text.IO as T

import Control.Monad.Apiary(ApiaryT, runApiaryTWith, ApiaryConfig)
import Data.Apiary.Extension
    ( Has, Extension, Extensions, getExtension, noExtension
    , Initializer, Initializer', initializer', (+>)
    )
import Data.Proxy(Proxy(..))

data Heroku = Heroku
    { herokuEnv    :: IORef (Maybe (H.HashMap T.Text T.Text))
    , herokuConfig :: HerokuConfig
    }

instance Extension Heroku

data HerokuConfig = HerokuConfig
    { defaultPort          :: Int
    , herokuExecutableName :: String
    , herokuAppName        :: Maybe String
    , herokuApiaryConfig   :: ApiaryConfig
    }

instance Default HerokuConfig where
    def = HerokuConfig 3000 "heroku" Nothing def

initHeroku :: MonadIO m => HerokuConfig -> Initializer' m Heroku
initHeroku conf = initializer' . liftIO $
    Heroku <$> newIORef Nothing <*> pure conf

-- | use this function instead of serverWith in heroku app. since 0.17.0.
--
-- @ runApiaryTWith id (run 3000) exts def $ foo @
--
-- to
--
-- @ runHerokuTWith id  run       exts def $ foo @
--
runHerokuTWith :: (MonadIO m, Monad actM)
               => (forall b. actM b -> IO b)
               -> (Int -> Wai.Application -> m a)
               -> Initializer m '[Heroku] exts
               -> HerokuConfig
               -> ApiaryT exts '[] actM m ()
               -> m a
runHerokuTWith runAct run ir conf m = do
    port <- liftIO $ fmap read (getEnv "PORT")
        `catch` (\(_::IOError) -> return $ defaultPort conf)
    runApiaryTWith runAct (run port) (initHeroku conf +> ir) (herokuApiaryConfig conf) m

runHerokuWith :: MonadIO m
              => (Int -> Wai.Application -> m a)
              -> Initializer m '[Heroku] exts
              -> HerokuConfig
              -> ApiaryT exts '[] IO m ()
              -> m a
runHerokuWith = runHerokuTWith id

-- | use this function instead of runApiary in heroku app. since 0.18.0.
--
-- this function provide:
--
-- * set port by PORT environment variable.
-- * getHerokuEnv function(get config from environment variable or @ heroku config @ command).
runHeroku :: MonadIO m
          => (Int -> Wai.Application -> m a)
          -> HerokuConfig
          -> ApiaryT '[Heroku] '[] IO m ()
          -> m a
runHeroku run = runHerokuWith run noExtension

getHerokuEnv' :: T.Text -- ^ heroku environment variable name
              -> Heroku -> IO (Maybe T.Text)
getHerokuEnv' envkey Heroku{..} = try (getEnv $ T.unpack envkey) >>= \case
    Right e                 -> return (Just $ T.pack e)
    Left (_::SomeException) -> readIORef herokuEnv >>= \case
        Just hm -> return $ H.lookup envkey hm
        Nothing -> do
            let args = ["config", "-s"] ++
                    maybe [] (\n -> ["--app", n]) (herokuAppName herokuConfig)
                cp   = proc (herokuExecutableName herokuConfig) args
            (_, Just hout, _, h) <- createProcess cp {std_out = CreatePipe}
            xc <- waitForProcess h
            if xc == ExitSuccess
            then do
                hm <- H.fromList . map (second T.tail . T.break  (== '=')) . T.lines
                    <$> T.hGetContents hout
                writeIORef herokuEnv (Just hm)
                return $ H.lookup envkey hm
            else Nothing <$ writeIORef herokuEnv (Just H.empty)


getHerokuEnv :: Has Heroku exts => T.Text -- ^ heroku environment variable name
             -> Extensions exts -> IO (Maybe T.Text)
getHerokuEnv envkey exts = getHerokuEnv' envkey (getExtension Proxy exts)
