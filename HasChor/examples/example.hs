{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE LambdaCase     #-}

module Main where

import Control.Concurrent
import Control.Concurrent.STM

main :: IO ()
main = do
    -- Create two TChans for the two expressions
    tchan1 <- newTChanIO
    tchan2 <- newTChanIO

    -- Spawn a thread to read from the TChans and execute the expressions
    _ <- forkIO $ executeBasedOnChannel tchan1 tchan2

    -- Write values to the TChans (you can replace these with your own expressions)
    atomically $ writeTChan tchan1 "Hello"
    atomically $ writeTChan tchan2 "World"

    -- Wait for a while to allow the thread to execute
    threadDelay 2000000

executeBasedOnChannel :: TChan String -> TChan String -> IO ()
executeBasedOnChannel tchan1 tchan2 = do
    -- Read from TChans using STM
    result1 <- atomically $ readTChan tchan1
    result2 <- atomically $ readTChan tchan2

    -- Choose which expression to execute based on the values read
    let expressionToExecute = if length result1 > length result2 then result1 else result2

    -- Execute the chosen expression (replace this with your own logic)
    putStrLn $ "Executing: " ++ expressionToExecute
