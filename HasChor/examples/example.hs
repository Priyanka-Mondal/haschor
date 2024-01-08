{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE LambdaCase     #-}

module Main where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Applicative (Alternative(..),(<|>))

-- A utility function to create a new TChan
newTChanIO' :: STM (TChan a)
newTChanIO' = newTChan

-- Read from the first non-empty TChan
readFromEitherTChan :: TChan a -> TChan a -> STM a
readFromEitherTChan chan1 chan2 = readTChan chan1 <|> readTChan chan2

main :: IO ()
main = do
    -- Create two TChan channels
    channel1 <- atomically newTChanIO'
    channel2 <- atomically newTChanIO'

    -- Write some values to the channels (for demonstration purposes)
    --atomically $ writeTChan channel1 "Hello"
    atomically $ writeTChan channel2 "World"

    -- Read from the first non-empty channel
    value <- atomically $ readFromEitherTChan channel1 channel2
    putStrLn $ "Received value: " ++ show value