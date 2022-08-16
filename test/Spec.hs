{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Main (main) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (withAsync)
import Control.Monad
import Data.ByteString
import Data.IORef
import qualified Network.HTTP.Client as HC
import qualified Network.HTTP.Client.WebSockets as HCWS
import Network.URI.Static
import qualified Network.WebSockets as WS
import Test.Hspec

main :: IO ()
main = hspec $
  describe "WebSockets via http-client" $ do
    it "should connect to an echo server" $ do
      pseudoTlsRef <- newIORef False
      mgr <-
        HC.newManager
          HC.defaultManagerSettings
            { HC.managerTlsConnection = do
                mkRawConnection <- HC.managerRawConnection HC.defaultManagerSettings
                pure $ \mha h p -> do
                  writeIORef pseudoTlsRef True
                  mkRawConnection mha h p
            }
      withEchoServer $ do
        HCWS.runClient mgr [uri|ws://localhost:12345|] testApp
        (`shouldBe` False) =<< readIORef pseudoTlsRef
        HCWS.runClient mgr [uri|wss://localhost:12345|] testApp
        (`shouldBe` True) =<< readIORef pseudoTlsRef
  where
    testApp conn = do
      let bs = "test" :: ByteString
      WS.sendTextData conn bs
      bs' <- WS.receiveData conn
      bs `shouldBe` bs'

    withEchoServer ma = withAsync runEchoServer (const $ threadDelay 100000 *> ma)
      where
        runEchoServer = WS.runServer "localhost" 12345 $ \pendingConn -> do
          conn <- WS.acceptRequest pendingConn
          forever $ WS.receive conn >>= WS.send conn
