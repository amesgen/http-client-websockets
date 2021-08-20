{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Main (main) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (withAsync)
import Control.Monad
import Data.ByteString
import Network.HTTP.Client.TLS (newTlsManager)
import Network.HTTP.Client.WebSockets as HCWS
import Network.URI.Static
import qualified Network.WebSockets as WS
import Test.Hspec

-- TODO test TLS websockets

main :: IO ()
main = hspec $
  describe "WebSockets via http-client" $ do
    it "should connect to an echo server" $ do
      mgr <- newTlsManager
      withEchoServer $
        HCWS.runClient mgr [uri|ws://localhost:12345|] $ \conn -> do
          let bs = "test" :: ByteString
          WS.sendTextData conn bs
          bs' <- WS.receiveData conn
          bs `shouldBe` bs'
  where
    withEchoServer ma = withAsync runEchoServer (const $ threadDelay 100000 *> ma)
      where
        runEchoServer = WS.runServer "localhost" 12345 $ \pendingConn -> do
          conn <- WS.acceptRequest pendingConn
          forever $ WS.receive conn >>= WS.send conn
