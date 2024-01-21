{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE LambdaCase     #-}

module Main where

import Choreography
import Data.Proxy
import Data.Time
import System.Environment
import Control.Concurrent (threadDelay)

buyer :: Proxy "buyer"
buyer = Proxy

seller :: Proxy "seller"
seller = Proxy

flea :: Proxy "flea"
flea = Proxy

-- | `bookseller` is a choreography that implements the bookseller protocol.
bookseller :: Choreo IO () --(Maybe Day @ "buyer")
bookseller = do
  -- the buyer node prompts the user to enter the title of the book to buy
  title <-
    buyer `locally` \_ -> do
      putStrLn "Enter the title of the book to buy"
      getLine
 
  title' <- (buyer, title) ~> seller
  
  seller `locally` \un -> do
      putStrLn (un title')
 
  price <- seller `locally` \un -> return $ priceOf (un title')
  price' <- (seller, price) ~> buyer

  pricep <- seller `locally` \un -> return $ priceOf' (un price)
  pricep' <- (buyer, price') ~> seller
  price'' <- (buyer, price') ~> flea 

  price'' <- flea `locally` \un -> return (un price'' +1)
  price1 <- (flea, price'') ~> seller 
  price'' <- flea `locally` \un -> return (un price'' +1)
  price1 <- (flea, price'') ~> seller 
  price'' <- flea `locally` \un -> return (un price'' +1)
  price2 <- (flea, price'') ~> seller 
  price'' <- flea `locally` \un -> return (un price'' +1)
  price3 <- (flea, price'') ~> seller 
  price'' <- flea `locally` \un -> return (un price'' +1)
  price4 <- (flea, price'') ~> seller 

  seller `locally` \un -> do
      putStrLn $ "seller done" ++ show (un pricep)
  seller `locally` \un -> do
      putStrLn $ "seller done too" ++ show (un pricep')
  seller `locally` \un -> do
      putStrLn $ "seller done toooo" ++ show (un price1)
  seller `locally` \un -> do
      putStrLn $ "seller done toooo" ++ show (un price2)
  seller `locally` \un -> do
      putStrLn $ "seller done toooo" ++ show (un price3)
  seller `locally` \un -> do
      putStrLn $ "seller done toooo" ++ show (un price4)


  buyer `locally` \un -> do
      putStrLn $ "buyer done" ++ show (un price')
  
  flea `locally` \un -> do
      putStrLn $ "flea done" ++ show (un price'')

  return ()

budget :: Int
budget = 100

priceOf :: String -> Int
priceOf "Types and Programming Languages" = 80
priceOf "Homotopy Type Theory"            = 120
priceOf "X"            = 12

priceOf' :: Int -> Int
priceOf' x =  helper x 0

helper :: Int -> Int -> Int
helper x y = if y < x*1000000000 
               then helper x (y+1)
               else y 

deliveryDateOf :: String -> Day
deliveryDateOf "Types and Programming Languages" = fromGregorian 2022 12 19
deliveryDateOf "Homotopy Type Theory"            = fromGregorian 2023 01 01
deliveryDateOf "X"            = fromGregorian 2024 09 09

main :: IO ()
main = do
  [loc] <- getArgs
  case loc of
    "buyer"  -> runChoreography cfg bookseller "buyer"
    "seller" -> runChoreography cfg bookseller "seller"
    "flea" -> runChoreography cfg bookseller "flea"
  return ()
  where
    cfg = mkHttpConfig [ ("buyer",  ("localhost", 4242))
                       , ("seller", ("localhost", 4343))
                       , ("flea", ("localhost", 4344))
                       ]