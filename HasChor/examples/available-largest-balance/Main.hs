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
  
  bal1' <- (b1, bal1) ~> tcb
  bal2' <- (b2, bal2) ~> tcb
  
  largest <- tcb `locally` \un -> do 
    if  un bal1' > un bal2'
        then return $ un bal1' 
        else return $ un bal2'

  tcb `locally` \un -> do 
    putStrLn $ "Largest is:" ++ show (un largest)
  
  hi <- client `locally` \un -> do 
    putStrLn "Type to start:" 
    readLn :: IO Int

  availBal <- sel (b1, bal1) (b2, bal2) client

  client `locally` \un -> do 
    putStrLn $ "Available balance(bal1/bal2):" ++ show (un availBal)

  larAv <- sel (tcb, largest) (client, availBal) client

  client `locally` \un -> do 
    putStrLn $ "Available balance (large/avail):" ++ show (un larAv)

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