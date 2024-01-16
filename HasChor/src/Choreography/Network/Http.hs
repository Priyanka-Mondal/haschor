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
import Text.Read (readEither, Lexeme (String))
import Control.Applicative (Alternative(..),(<|>))
import Control.Concurrent.STM
import System.Timeout
import System.IO.Unsafe (unsafePerformIO)

import Debug.Trace
import Data.Bits (Bits(xor))
import GHC.Base (failIO)
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
-- if readTChan l1 == readTChan l2 then readTChan l2 else readTChan l2
--checkAndRead :: forall a. Read a => TChan a -> STM a
checkAndRead l = do
                 cond <- isEmptyTChan l
                 if not cond
                   then readTChan l
                   else return "-1"

class DefaultType a where
    getDefault :: a -> a

instance DefaultType (TVar a) where
    getDefault x = x


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
       res <- runClientM (send' self $ show a) (mkClientEnv mgr (locToUrl cfg ! l))
       case res of
        Left err -> putStrLn $ "ErrorSS : " ++ show err
        Right _  -> return ()
      handler' (Recv l)   = liftSTM $ read <$> readTChan (chans ! l)
      handler' (PairRecv l1 l2)  = liftSTM $ read <$> readEither (chans ! l1) (chans ! l2) 
      handler' (RecvCompare l1 l2)  = liftSTM $ read <$> readCompare (chans ! l1) (chans ! l2)
      handler' (MayRecv l)   = liftSTM $ read <$> checkAndRead (chans ! l)
      handler' (MaySend a l)  = liftIO $ do
       res <- runClientM (send' self $ show a) (mkClientEnv mgr (locToUrl cfg ! l))
       case res of
        Left err -> putStrLn "(Program continued without an Input from me)"
        Right _  -> return ()
      handler' (BCast a)  = mapM_ handler' $ fmap (Send a) (locs cfg)
      
    

    --readEither :: forall a.(Read a, Eq a) => TChan a -> TChan a -> STM a
    readEither l1 l2 =  do 
          newchan <- newTChan
          writeTChan newchan "-1"
          cond1 <- isEmptyTChan l1
          if cond1
            then do
              cond2 <- isEmptyTChan l2
              if cond2 
                then readTChan newchan
                else readTChan l2
            else do
              one <- peekTChan l1
              if one == "-1"
                then do 
                  cond2 <- isEmptyTChan l2
                  if cond2 
                    then readTChan l1 ----- <<
                    else readTChan l2
                else readTChan l1   ----- <<
  
    --readCompare :: forall a. (Read a, Eq a) => TChan a -> TChan a -> STM a
    readCompare l1 l2 = do
          newchan <- newTChan
          writeTChan newchan "-1"
          cond1 <- isEmptyTChan l1
          if not cond1
           then do 
            cond2 <- isEmptyTChan l2
            if not cond2
              then do 
                one <- peekTChan l1
                two <- readTChan l2
                if one == two then readTChan l1 else readTChan newchan
              else readTChan newchan
           else readTChan newchan

   {-- readCompare :: forall a b. (Read a, Eq a, Read b, Eq b) => TChan a -> TChan a -> b -> IO (Either (STM a) b)
    readCompare l1 l2 failMessage = do
     cond1 <- atomically $ isEmptyTChan l1
     newchan <- newTChan
     if not cond1
        then do
            cond2 <- atomically $ isEmptyTChan l2
            if not cond2
                then return (Left (readTChan l2))
                else return (Right failMessage)
        else return (Right failMessage)
     --}
   {--
    readEither :: forall a. Read a => TChan a -> TChan a -> STM a
    readEither l1 l2 =   readTChan l1 `orElse` readTChan l2
    --}


    api :: Proxy API 
    api = Proxy

    send' :: LocTm -> String -> ClientM NoContent
    send' = client api

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

{--
The source file extension must be ".hs"
Can not use STDIN handle while debugging.
Creating tasks.json.
Shortcut keys
F5 : start / continue debugging
F6 : show command menu
Shift + F6 : stop watch
F7 : clean & build
F8 : start test
F9 : put a breakpoint on the current line
Shift + F9 : put a breakpoint on the current column
F10 : step next
F11 : step into
Install
Stack
Install haskell-dap, ghci-dap, haskell-debug-adapter at once.

$ stack update
$
$ stack install haskell-dap ghci-dap haskell-debug-adapter
$
$ ghci-dap --version
[DAP][INFO] start ghci-dap-0.0.XX.0.
The Glorious Glasgow Haskell Compilation System, version X.X.X
$
$ haskell-debug-adapter --version
VERSION: haskell-debug-adapter-0.0.XX.0
$


--}

{--


    --checkBoth :: forall a. (Read a, Eq a) => String -> String -> STM a
    checkBoth a1 a2 = do
     newchan1 <- newTChan
     writeTChan newchan1 a1
     newchan2 <- newTChan
     writeTChan newchan2 a2
     one <- peekTChan newchan1
     two <- peekTChan newchan2
     if a1 ==  read "-1"
        then readTChan newchan2
        else readTChan newchan1
--}