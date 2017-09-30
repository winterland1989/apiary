{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DataKinds #-}

import Web.Apiary
import Web.Apiary.WebSockets
import Network.Wai.Handler.Warp
import qualified Data.Text as T
import Data.Apiary.Routing.Dict(get)
import Control.Concurrent
import Language.Haskell.TH
import System.FilePath
import System.Directory

main :: IO ()
main = do
    setCurrentDirectory $(location >>= stringE . takeDirectory . loc_filename)
    runApiary (run 3000) def $ do
        [capture|/i::Int|] . webSockets $ servApp . get [key|i|]
        root $ actionWithWebSockets (const $ servApp 0) (file "websockets.html" Nothing)

servApp :: Int -> PendingConnection -> IO ()
servApp st pc = do
    c <- acceptRequest pc
    go c st
  where
    go c i
        | i > 10 = sendClose c ("Close" :: T.Text)
        | otherwise = do
            sendTextData c (T.pack $ show i)
            liftIO $ putStrLn "send"
            threadDelay (10 ^ (6 :: Int))
            go c (succ i)
