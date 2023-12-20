{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE LambdaCase     #-}

module Main where

import Choreography
import Data.Proxy
import Data.Time
import System.Environment

import Control.Monad.IO.Class

import Control.Concurrent

import Control.Concurrent.STM

import Control.Monad (replicateM_, forever, void)

import Control.Applicative ((<|>))

--import Say

buyer :: Proxy "buyer"
buyer = Proxy

seller :: Proxy "seller"
seller = Proxy

flee :: Proxy "flee"
flee = Proxy

--eachThread :: [IO()] -> 

{--
let ios = [(write 'a'), (write 'C')]
  forks <- mapM forkIO ios
  threadDelay 1000000
  putStrLn "Stop printing as and bs!"
  _ <- mapM_ killThread forks -- or use mapM_ for no returning result effect
  pure () -- return ()
  where
    write c = do 
      putChar c
      write c
--}

{--
result <- newEmptyMVar

    forkIO (do
        sleepMs 100
        putStrLn "Calculated result!"
        putMVar result 42)

    putStrLn "Waiting..."
    value <- takeMVar result
    putStrLn ("The answer is: " ++ show value)
--}
{--
counter <- newMVar 0

    let increment = do
            count <- takeMVar counter
            putMVar counter $! count + 1
        incrementer = do
            replicateM 1000 increment
            return ()

    threads <- replicateM 5 (forkIO incrementer)

    sleepMs 100
    count <- takeMVar counter
    print count
--}
-- if you want the results use mapM, if you dont use mapM_
sleepMs n = threadDelay (n * 1000)


{--
nonDuplicatedTest = do
    messages1 <- newChan
    messages2 <- newChan 
    forkIO (messageWriter messages1 "First")
    forkIO (messageWriter messages2 "Second")
    forkIO (messageReader messages1 messages2 "Reader")
    return ()

messageWriter channel name = do
    if name == "First" then sleepMs 5 else sleepMs 100
    writeChan channel ("Hi There! "  ++ name)

messageReader :: Chan [Char] -> p -> [Char] -> IO ()
messageReader channel1 channel2 name = do
    msg <- readChan channel1
    putStrLn (name ++ " read: " ++ msg)

duplicatedTest = do
    broadcast <- newChan
    forkIO (broadcastReader broadcast "Third")
    forkIO (broadcastReader broadcast "Fourth")
    writeChan broadcast "Bye!"

broadcastReader channel name = do
    channel' <- dupChan channel
    --messageReader channel' name
    msg <- readChan channel
    putStrLn (name ++ " read:==> " ++ msg)
    sleepMs 100
    --}


main = do
  aliceVar <- newTVarIO 0
  bobVar <- newTVarIO 0
  charlieVar <- newTVarIO 0

  payThread aliceVar 1000 2 "Alice"
  payThread bobVar   1000 6 "Bob"

  stt <- atomically $ transfer 20 aliceVar charlieVar
           <|> transfer 20 bobVar charlieVar 
  finalAlice <- readTVarIO aliceVar
  finalBob <- readTVarIO bobVar
  finalCharlie <- readTVarIO charlieVar ---atomically $ readTVar charlieVar

  putStrLn $ "Final Alice: " ++ show finalAlice
  putStrLn $ "Final Bob: " ++ show finalBob
  putStrLn $ "Final Charlie: " ++ show finalCharlie

payThread :: TVar Int -> Int -> Int -> String-> IO ()
payThread var interval amount thd= void $ forkIO $ forever $ do --forever deleted
  threadDelay interval
  current <- readTVarIO var --atomically $ readTVar var
  putStrLn $ "Adding++ " ++ thd ++ "::"++ show (current+amount)
  
  atomically $ do
    current <- readTVar var
    writeTVar var (current + amount)

transfer :: Int -> TVar Int -> TVar Int -> STM ()
transfer amount fromVar toVar = do
  currentFrom <- readTVar fromVar
  check (currentFrom >= amount)
  writeTVar fromVar (currentFrom - amount)
  currentTo <- readTVar toVar
  writeTVar toVar (currentTo + amount)