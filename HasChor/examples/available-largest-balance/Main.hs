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
      putStrLn "Enter bal1::"
      readLn :: IO Int

  bal2 <-
    b2 `locally` \_ -> do
      putStrLn "Enter bal2::"
      readLn :: IO Int
  
  client `locally` \un -> do 
    putStrLn "Type to start at client:" 
    getLine 

  bal1' <-
    b1 `locally` \_ -> do
      putStrLn "Again Enter bal1::"
      readLn :: IO Int

  s <- sel (b2, bal2) (b1, bal1) client

  bal2' <-
    b2 `locally` \_ -> do
      putStrLn "Again Enter bal2::"
      readLn :: IO Int
  
  client `locally` \un  -> do
      putStrLn $ "balance1 " ++ (show (un s))

  s' <- sel (b2, bal2') (b1, bal1') client

  client `locally` \un  -> do
      putStrLn $ "balance2 " ++ (show (un s'))

  bal3 <-
    b2 `locally` \_ -> do
      putStrLn "Again Enter bal3::"
      readLn :: IO Int
  
  s'' <- sel (b2, bal3) (b1, bal1') client
  
  client `locally` \un  -> do
      putStrLn $ "balance3 " ++ (show (un s''))

  sec <- (b1, bal1') ~> client
  client `locally` \un  -> do
      putStrLn $ "balance3 " ++ (show (un sec))

  b1 `locally` \_ -> do
      putStrLn "b1: I am done!"

  b2 `locally` \_ -> do
      putStrLn "b2: I am done!!!"

  return ()
  
main :: IO ()
main = do
  [loc] <- getArgs
  case loc of
    "client" -> runChoreography cfg largestAvailableBalance "client"
    "b1" -> runChoreography cfg largestAvailableBalance "b1"
    "b2" -> runChoreography cfg largestAvailableBalance "b2"
  return ()
  where
    cfg = mkHttpConfig [ ("client",  ("localhost", 4243))
                        , ("b1", ("localhost", 4341))
                        , ("b2", ("localhost", 4342))
                       ]


balanceOfBank1 :: String -> Int
balanceOfBank1 "Alice" = 80
balanceOfBank1 "Bob"   = 120
balanceOfBank1 "Carol" = 20
balanceOfBank1 "y"     = 50
balanceOfBank1 "z"     = 150

balanceOfBank2 :: String -> Int
balanceOfBank2 "Alice" = 800
balanceOfBank2 "Bob"   = 40
balanceOfBank2 "Carol" = 90
balanceOfBank2 "y"     = 1230
balanceOfBank2 "z"     = 90000