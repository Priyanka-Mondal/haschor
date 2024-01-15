{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE LambdaCase     #-}

module Main where

import Choreography
import Data.Proxy
import Data.Time
import System.Environment
import Text.Read (Lexeme(String))

client :: Proxy "client"
client = Proxy

b1 :: Proxy "b1"
b1 = Proxy

b2 :: Proxy "b2"
b2 = Proxy


tcb :: Proxy "tcb"
tcb = Proxy


-- | FLAQR example 2
largestAvailableBalance :: Choreo IO () --(Maybe Day @ "client")
largestAvailableBalance = do
  bal1 <-
    b1 `locally` \_ -> do
      putStrLn "Enter the bal1::"
      getLine

  bal2 <-
    b2 `locally` \_ -> do
      putStrLn "Enter the bal2::"
      getLine
  
  bal1' <- (b1, bal1) ~> tcb
  bal2' <- (b2, bal2) ~> tcb
  
  largest <- tcb `locally` \un -> do 
    if un bal1' > un bal2' 
        then return $ un bal1' 
        else return $ un bal2'

  tcb `locally` \un -> do 
    putStrLn $ "Largest is:" ++ show (un largest)
  
  hi <- client `locally` \un -> do 
    putStrLn "Type if you are ready" 
    getLine

  availBal <- sel (b1, bal1) (b2, bal2) client

  client `locally` \un -> do 
    putStrLn $ "Available balance:" ++ show (un availBal)

  lavbal <- sel (client, availBal) (tcb, largest) client

  client `locally` \un -> do 
    putStrLn $ "Balance =" ++ show (un lavbal)

  return ()
  
main :: IO ()
main = do
  [loc] <- getArgs
  case loc of
    "client" -> runChoreography cfg largestAvailableBalance "client"
    "b1" -> runChoreography cfg largestAvailableBalance "b1"
    "b2" -> runChoreography cfg largestAvailableBalance "b2"
    "tcb" -> runChoreography cfg largestAvailableBalance "tcb"
  return ()
  where
    cfg = mkHttpConfig [ ("client",  ("localhost", 4240))
                       , ("b1", ("localhost", 4341))
                       , ("b2", ("localhost", 4342))
                       , ("tcb", ("localhost", 4343))
                       ]
