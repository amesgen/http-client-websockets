{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Main (main) where

import Data.ByteString
import Data.Foldable (for_)
import Network.HTTP.Client.TLS (newTlsManager)
import Network.HTTP.Client.WebSockets as HCWS
import Network.URI.Static
import qualified Network.WebSockets as WS
import Test.Hspec

main :: IO ()
main = hspec $
  describe "WebSockets via http-client" $ do
    it "should connect to an echo server" $ do
      mgr <- newTlsManager
      for_ [[uri|ws://echo.websocket.org|], [uri|wss://echo.websocket.org|]] $ \u ->
        HCWS.runClient mgr u $ \conn -> do
          let bs = "test" :: ByteString
          WS.sendTextData conn bs
          bs' <- WS.receiveData conn
          bs `shouldBe` bs'
