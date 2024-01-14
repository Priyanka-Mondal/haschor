{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE LambdaCase     #-}

module Main where

import Control.Concurrent
import Control.Concurrent.STM

readCompare :: forall a. (Read a, Eq a) => TChan a -> TChan a -> a -> STM a
readCompare l1 l2 defaultValue = do
    newchan <- newTChan
    writeTChan newchan defaultValue
    writeTChan l1 1
    writeTChan l2 1
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

main :: IO ()
main = do
    let defaultValue = 0 :: Int  -- Change the type and value as needed

    -- Create TChans within STM
    (l1, l2) <- atomically $ do
        chan1 <- newTChan
        chan2 <- newTChan
        return (chan1, chan2)

    result <- atomically $ readCompare l1 l2 defaultValue
    print result



{--

 readCompare :: forall a. (Read a, Eq a, DefaultType a) => TChan a -> TChan a -> a -> STM (Maybe a)
    readCompare l1 l2 def = do
     --let defaultValue = getDefault 90
     result <- checkLoop 0
     if result
        then readTChan l1
        else return defaultValue
     where
      checkLoop :: Int -> STM Bool
      checkLoop attempts
        | attempts >= 100 = return False  -- Stop after 100 attempts
        | otherwise = do
            cond1 <- isEmptyTChan l1
            if not cond1
                then return True
                else do
                    cond2 <- isEmptyTChan l2
                    if not cond2
                        then return True
                        else do
                            retry
                            checkLoop (attempts + 1)
--}