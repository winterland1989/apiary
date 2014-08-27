{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

import System.Environment
import Web.Spock
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL

main :: IO ()
main = do
    port:_ <- getArgs
    spockT (read port) id $ do
        get "/echo/hello-world" $ text "Hello World"
        get "/echo/plain/:param/:int" $ do
            Just p <- param "param"
            Just i <- param "int"
            lazyBytes . TL.encodeUtf8 . TL.concat $ replicate i p

        subcomponent "/deep/foo/bar/baz" $ do
            get "0" $ text "deep"
            get "1" $ text "deep"
            get "2" $ text "deep"
            get "3" $ text "deep"
            get "4" $ text "deep"
            get "5" $ text "deep"
            get "6" $ text "deep"
            get "7" $ text "deep"
            get "8" $ text "deep"
            get "9" $ text "deep"
            get "10" $ text "deep"
            get "11" $ text "deep"
            get "12" $ text "deep"
            get "13" $ text "deep"
            get "14" $ text "deep"
            get "15" $ text "deep"
            get "16" $ text "deep"
            get "17" $ text "deep"
            get "18" $ text "deep"
            get "19" $ text "deep"
            get "20" $ text "deep"
            get "21" $ text "deep"
            get "22" $ text "deep"
            get "23" $ text "deep"
            get "24" $ text "deep"
            get "25" $ text "deep"
            get "26" $ text "deep"
            get "27" $ text "deep"
            get "28" $ text "deep"
            get "29" $ text "deep"
            get "30" $ text "deep"
            get "31" $ text "deep"
            get "32" $ text "deep"
            get "33" $ text "deep"
            get "34" $ text "deep"
            get "35" $ text "deep"
            get "36" $ text "deep"
            get "37" $ text "deep"
            get "38" $ text "deep"
            get "39" $ text "deep"
            get "40" $ text "deep"
            get "41" $ text "deep"
            get "42" $ text "deep"
            get "43" $ text "deep"
            get "44" $ text "deep"
            get "45" $ text "deep"
            get "46" $ text "deep"
            get "47" $ text "deep"
            get "48" $ text "deep"
            get "49" $ text "deep"
            get "50" $ text "deep"
            get "51" $ text "deep"
            get "52" $ text "deep"
            get "53" $ text "deep"
            get "54" $ text "deep"
            get "55" $ text "deep"
            get "56" $ text "deep"
            get "57" $ text "deep"
            get "58" $ text "deep"
            get "59" $ text "deep"
            get "60" $ text "deep"
            get "61" $ text "deep"
            get "62" $ text "deep"
            get "63" $ text "deep"
            get "64" $ text "deep"
            get "65" $ text "deep"
            get "66" $ text "deep"
            get "67" $ text "deep"
            get "68" $ text "deep"
            get "69" $ text "deep"
            get "70" $ text "deep"
            get "71" $ text "deep"
            get "72" $ text "deep"
            get "73" $ text "deep"
            get "74" $ text "deep"
            get "75" $ text "deep"
            get "76" $ text "deep"
            get "77" $ text "deep"
            get "78" $ text "deep"
            get "79" $ text "deep"
            get "80" $ text "deep"
            get "81" $ text "deep"
            get "82" $ text "deep"
            get "83" $ text "deep"
            get "84" $ text "deep"
            get "85" $ text "deep"
            get "86" $ text "deep"
            get "87" $ text "deep"
            get "88" $ text "deep"
            get "89" $ text "deep"
            get "90" $ text "deep"
            get "91" $ text "deep"
            get "92" $ text "deep"
            get "93" $ text "deep"
            get "94" $ text "deep"
            get "95" $ text "deep"
            get "96" $ text "deep"
            get "97" $ text "deep"
            get "98" $ text "deep"
            get "99" $ text "deep"
            get "100" $ text "deep"

        get "after" $ text "after"
