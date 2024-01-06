{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs     #-}

-- | This module implments the HTTP message transport backend for the `Network`
-- monad.
module Choreography.Network.Http where

import Choreography.Location
import Choreography.Network hiding (run)
import Data.ByteString (fromStrict)
import Data.Proxy (Proxy(..))
import Data.HashMap.Strict (HashMap, (!))
import Data.HashMap.Strict qualified as HashMap
import Network.HTTP.Client (Manager, defaultManagerSettings, newManager)
import Servant.API
import Servant.Client (ClientM, client, runClientM, BaseUrl(..), mkClientEnv, Scheme(..))
import Servant.Server (Handler, Server, serve)
import Control.Concurrent
import Control.Concurrent.Chan
import Control.Monad
import Control.Monad.Freer
import Control.Monad.IO.Class
import Network.Wai.Handler.Warp (run)
import Text.Read (readEither)
import Control.Applicative (Alternative(..),(<|>))
import Control.Concurrent.STM
import System.IO.Unsafe (unsafePerformIO)
-- * Servant API

type API = "send" :> Capture "from" LocTm :> ReqBody '[PlainText] String :> PostNoContent

-- * Http configuration

-- | The HTTP backend configuration specifies how locations are mapped to
-- network hosts and ports.
newtype HttpConfig = HttpConfig
  { locToUrl :: HashMap LocTm BaseUrl
  }

type Host = String
type Port = Int

-- | Create a HTTP backend configuration from a association list that maps
-- locations to network hosts and ports.
mkHttpConfig :: [(LocTm, (Host, Port))] -> HttpConfig
mkHttpConfig = HttpConfig . HashMap.fromList . fmap (fmap f)
  where
    f :: (Host, Port) -> BaseUrl
    f (host, port) = BaseUrl
      { baseUrlScheme = Http
      , baseUrlHost = host
      , baseUrlPort = port
      , baseUrlPath = ""
      }

locs :: HttpConfig -> [LocTm]
locs = HashMap.keys . locToUrl

-- * Receiving channels

type RecvChans = HashMap LocTm (TChan String)

mkRecvChans :: HttpConfig -> STM RecvChans
mkRecvChans cfg = foldM f HashMap.empty (locs cfg)
  where
    f :: HashMap LocTm (TChan String) -> LocTm
      -> STM (HashMap LocTm (TChan String))
    f hm l = do
      c <- newTChan
      return $ HashMap.insert l c hm

liftSTM :: MonadIO m => STM a -> m a
liftSTM = liftIO . atomically
--liftIO :: forall (m :: Type -> Type) a. MonadIO m => IO a -> m a
--atomically :: forall a. STM a -> IO a

--(read <$> readTChan (chans ! l1)) <|> (read <$> readTChan (chans ! l2))
stmBoolToBool :: STM Bool -> Bool
stmBoolToBool stmAction = unsafePerformIO $ atomically stmAction

checkAndRead :: forall a. Read a => LocTm -> RecvChans -> STM a
checkAndRead l chans = do
                        cond <- atomically $ isEmptyTChan (chans ! l)
                        if cond then read <$> readTChan (chans ! l) else return ()
--  --check (not (stmBoolToBool (isEmptyTChan (chans ! l))))

runNetworkHttp :: (MonadIO m) => HttpConfig -> LocTm -> Network m a -> m a
runNetworkHttp cfg self prog = do
  mgr <- liftIO $ newManager defaultManagerSettings
  chans <- liftSTM $ mkRecvChans cfg
  recvT <- liftIO $ forkIO (recvThread cfg chans)
  result <- runNetworkMain mgr chans prog
  liftIO $ threadDelay 1000000 -- wait until all outstanding requests to be completed
  liftIO $ killThread recvT
  return result
  where
    runNetworkMain :: (MonadIO m) => Manager -> RecvChans -> Network m a -> m a
    runNetworkMain mgr chans = interpFreer handler' where
      handler' :: (MonadIO m) => NetworkSig m a -> m a
      handler' (Run m)    = m
      handler' (Send a l) = liftIO $ do
       res <- runClientM (send self $ show a) (mkClientEnv mgr (locToUrl cfg ! l))
       case res of
        Left err -> putStrLn $ "Error : " ++ show err
        Right _  -> return ()
      handler' (Recv l)   = liftSTM $ read <$> readTChan (chans ! l)
      handler' (PairRecv l1 l2)  = liftSTM $ readTwo l1 l2 chans
      handler' (BCast a)  = mapM_ handler' $ fmap (Send a) (locs cfg)
      
    readTwo :: forall a. Read a => LocTm -> LocTm -> RecvChans -> STM a
    readTwo l1 l2 chans =  checkAndRead l1 chans <|> checkAndRead l2 chans

    api :: Proxy API 
    api = Proxy

    send :: LocTm -> String -> ClientM NoContent
    send = client api

    server :: RecvChans -> Server API
    server chans = handler
      where
       handler :: LocTm -> String -> Handler NoContent
       handler rmt msg = do
        liftSTM $ writeTChan (chans ! rmt) msg
        return NoContent

    recvThread :: HttpConfig -> RecvChans -> IO ()
    recvThread cfg chans = run (baseUrlPort $ locToUrl cfg ! self ) (serve api $ server chans)


    

instance Backend HttpConfig where
  runNetwork = runNetworkHttp
