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
    ( STM,
      TChan,
      TQueue,
      newTChan,
      atomically,
      isEmptyTChan,
      readTChan,
      peekTChan,
      writeTChan,
      newTQueue,
      readTQueue,
      writeTQueue )
import System.Timeout
import System.IO.Unsafe (unsafePerformIO)
import Debug.Trace
import Data.Bits (Bits(xor))
import GHC.Base (failIO)


type API = "send" :> Capture "from" LocTm :> ReqBody '[PlainText] String :> PostNoContent

-- * Http configuration
-- | The HTTP backend configuration specifies how locations are mapped to
-- network hosts and ports.


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

--locs :: HttpConfig -> [LocTm]
--locs = HashMap.keys . locToUrl

--uniquePairs ::Eq a => [a] -> [(a, a)]
--uniquePairs xs = [(x, y) | x <- xs, y <- xs, x /= y]

type ThdId = HashMap (LocTm,LocTm) ThreadId
writeThdId :: LocTm -> LocTm -> ThreadId ->ThdId -> IO ThdId
writeThdId s r tid hm = return $ HashMap.insert (s,r) tid hm

mkThdId :: HttpConfig -> IO ThdId
mkThdId cfg = foldM f HashMap.empty (uniquePairs (locs cfg))
  where
    f :: ThdId -> (LocTm,LocTm)
      -> IO ThdId
    f hm (s,r) = do
      temp <- forkIO (putStrLn "")
      let ret = HashMap.insert (s,r) temp hm
      killThread temp
      return ret




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

checkAndRead1 :: String -> TChan String -> STM String
checkAndRead1 a l = do
                      if a == "-1"
                        then do 
                          cond <- isEmptyTChan l
                          if not cond
                            then readTChan l
                            else return a
                        else return a
                     
checkAndRead2 :: TChan String -> String -> STM String
checkAndRead2 l a = do
                  cond <- isEmptyTChan l
                  if not cond
                    then do 
                      one <- peekTChan l
                      if one == "-1"
                       then return a
                       else readTChan l 
                    else return a

messageWriter :: TChan String -> String -> STM ()
messageWriter channel name = do
    writeTChan channel name

--messageReader :: TChan a -> STM ()
--messageReader channel = do 
--  readTChan channel
    

runNetworkHttp :: (MonadIO m) => HttpConfig -> IO ChanMap -> LocTm -> Network m a -> m a
runNetworkHttp cfg chanmapS self prog = do
  mgr <- liftIO $ newManager defaultManagerSettings
  chans <- liftSTM $ mkRecvChans cfg
  thdids <- liftIO $ mkThdId cfg
  chanmaps <- liftIO chanmapS
  recvT <- liftIO $ forkIO (recvThread cfg chans)
  result <- runNetworkMain mgr chans chanmaps thdids prog 
  liftIO $ threadDelay 1000000 -- wait until all outstanding requests to be completed
  liftIO $ killThread recvT
  return result
  where
    runNetworkMain :: (MonadIO m) => Manager -> RecvChans -> ChanMap -> ThdId -> Network m a -> m a
    runNetworkMain mgr chans chanmaps thdids = interpFreer handler' where
      handler' :: (MonadIO m) => NetworkSig m a -> m a
      handler' (Run m)    = m
      handler' (Send a l) = liftIO $ do
       newchan <- atomically newTChan
       newmvar <- newEmptyMVar
       putMVar newmvar (show a)
       recvT <- forkIO (atomically $ messageWriter newchan (show a))
       --thdids <- liftIO $ writeThdId self l recvT thdids
       --wrt <- atomically $ writeTChan newchan (show a)
       chanmap <- liftIO $ writeChanMap self l newchan chanmaps
       --putStrLn $ "here-------------->" ++ show (chanmap ! (l,self)) ++" "
       --ret1 <- atomically $ readTChan (chanmap ! (self,l)) 
       --ret2 <- atomically $ readTChan (chanmap ! (l,self)) 
       --putStrLn $ "fetch1-------------->" ++ ret1
       putStrLn $ "fetch2-------------->" 
       --res <- runClientM (send' self $ show a) (mkClientEnv mgr (locToUrl cfg ! l))
       --case res of
       -- Left err ->putStrLn "(did not work)"
       -- Right _  -> return ()
      handler' (Recv l)   = liftIO $ do 
        putStrLn "fetch"
        --recvT <- forkIO (recvThread cfg chans)
        let newchan = chanmaps ! (self,l)
        putStrLn "fetch0.1"
        ret1 <- atomically $ readTChan newchan
        putStrLn "fetch0.1"
        ret2 <- atomically $ readTChan (chanmaps ! (l,self))
        putStrLn "fetch1.1"
        putStrLn $ "fetch1-------------->" ++ ret1
        putStrLn $ "fetch2-------------->" ++ ret2
        --mapM_ (print.fst) (HashMap.toList chanmap)
        print self
        return (read ret1)
        --liftIO $ killThread (thdids ! (l,self))
        --return ret 
      handler' (PairRecv l1 l2)  = liftSTM $ read <$> readEither (chans ! l1) (chans ! l2) 
      handler' (RecvCompare l1 l2)  = liftSTM $ read <$> readCompare (chans ! l1) (chans ! l2)
      handler' (MayRecv1 a l)   = liftSTM $ read <$> checkAndRead1 (show a) (chans ! l)
      handler' (MayRecv2 l a)   = do
        liftIO $ threadDelay 1000000
        liftSTM $ read <$> checkAndRead2 (chans ! l) (show a)
      handler' (MaySend a l)  = liftIO $ do
       res <- runClientM (send' self $ show a) (mkClientEnv mgr (locToUrl cfg ! l))
       case res of
        Left err -> putStrLn "(Program continued without my Input)"
        Right _  -> return ()
      handler' (BCast a)  = mapM_ handler' $ fmap (Send a) (locs cfg)
      
     --readEither :: forall a.(Read a, Eq a) => TChan a -> TChan a -> STM a
    readEither :: TChan String -> TChan String -> STM String
    readEither l1 l2 =  do 
          cond1 <- isEmptyTChan l1
          if cond1
            then do
              cond2 <- isEmptyTChan l2
              if cond2 
                then return "-1"
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



type RecvQueue = HashMap (LocTm,LocTm) (TQueue String)


makeQueues :: HttpConfig -> STM RecvQueue
makeQueues cfg = foldM f HashMap.empty (uniquePairs (locs cfg))
  where
    f :: HashMap (LocTm,LocTm) (TQueue String) -> (LocTm,LocTm)
      -> STM (HashMap (LocTm,LocTm) (TQueue String))
    f hm pair = do 
      newQ <- newTQueue
      return $ HashMap.insert pair newQ hm

readQueue :: RecvQueue -> (LocTm, LocTm) -> STM String
readQueue recvq (l1, l2) =   readTQueue q 
   where 
    q = recvq ! (l1,l2)

writeQueue :: RecvQueue -> (LocTm, LocTm) -> String -> STM ()
writeQueue recvq pair =  writeTQueue (recvq ! pair)
     
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
