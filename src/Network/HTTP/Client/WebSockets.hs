{-# LANGUAGE LambdaCase #-}

-- | Glue code for [http-client](https://hackage.haskell.org/package/http-client)
--   and [websockets](https://hackage.haskell.org/package/websockets).
--
--   This module is intended to be imported @qualified@.
--
--   If you want to use TLS-secured WebSockets (via the @wss@ scheme)
--   you need to supply a 'Manager' which supports TLS, for example
--   from [http-client-tls](https://hackage.haskell.org/package/http-client-tls)
--   or [http-client-openssl](https://hackage.haskell.org/package/http-client-openssl).
--
--   == Example
--   >>> :set -XOverloadedStrings
--   >>> :set -XQuasiQuotes
--   >>>
--   >>> import Network.HTTP.Client (Manager, defaultManagerSettings)
--   >>> import qualified Network.WebSockets as WS
--   >>> import qualified Network.HTTP.Client.WebSockets as HCWS
--   >>> import Network.URI.Static
--   >>> import Data.ByteString (ByteString)
--   >>>
--   >>> :{
--       runEchoExample :: Manager -> IO ByteString
--       runEchoExample mgr = HCWS.runClient mgr echoUri $ \conn -> do
--           WS.sendTextData conn ("hello there" :: ByteString)
--           msg <- WS.receiveData conn
--           pure (msg :: ByteString)
--         where
--           echoUri = [uri|wss://echo.websocket.org|]
--   :}
module Network.HTTP.Client.WebSockets
  ( runClient,
    runClientWith,
    runClientWithRequest,
  )
where

import Control.Exception (throwIO)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Client.Internal as HTTP
import Network.URI (URI (..))
import qualified Network.WebSockets as WS
import qualified Network.WebSockets.Stream as WS

runClient ::
  -- | 'HTTP.Manager' to use to establish the connection
  HTTP.Manager ->
  -- | 'URI' to connect to. Only the schemes @ws@ and @wss@ are valid.
  URI ->
  -- | Client application
  WS.ClientApp a ->
  IO a
runClient mgr uri = runClientWith mgr uri WS.defaultConnectionOptions []

runClientWith ::
  -- | 'HTTP.Manager' to use to establish the connection
  HTTP.Manager ->
  -- | 'URI' to connect to. Only the schemes @ws@ and @wss@ are valid.
  URI ->
  -- | Options
  WS.ConnectionOptions ->
  -- | Custom headers to send
  WS.Headers ->
  -- | Client application
  WS.ClientApp a ->
  IO a
runClientWith mgr uri connOpts headers app = do
  httpScheme <- case uriScheme uri of
    "ws:" -> pure "http:"
    "wss:" -> pure "https:"
    s -> fail $ "invalid WebSockets scheme: " <> s
  req <- HTTP.requestFromURI uri {uriScheme = httpScheme}
  runClientWithRequest mgr (req {HTTP.requestHeaders = headers}) connOpts app

runClientWithRequest ::
  -- | 'HTTP.Manager' to use to establish the connection
  HTTP.Manager ->
  -- | 'HTTP.Request' to use to open the connection, content will be ignored.
  HTTP.Request ->
  -- | Options
  WS.ConnectionOptions ->
  -- | Client application
  WS.ClientApp a ->
  IO a
runClientWithRequest mgr req connOpts app = do
  HTTP.withConnection req mgr $ \conn -> do
    host <- toStringUtf8 $ HTTP.host req
    path <- toStringUtf8 $ HTTP.path req <> HTTP.queryString req
    let read = do
          bs <- HTTP.connectionRead conn
          pure $ if B.null bs then Nothing else Just bs
        write = \case
          Nothing -> HTTP.connectionClose conn
          Just bs -> HTTP.connectionWrite conn $ LB.toStrict bs
    stream <- WS.makeStream read write
    WS.runClientWithStream stream host path connOpts (HTTP.requestHeaders req) app
  where
    toStringUtf8 = fmap T.unpack . either throwIO pure . T.decodeUtf8'
