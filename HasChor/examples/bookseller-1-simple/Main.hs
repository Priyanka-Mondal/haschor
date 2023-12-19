{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE LambdaCase     #-}

module Main where

import Choreography
import Data.Proxy
import Data.Time
import System.Environment

buyer :: Proxy "buyer"
buyer = Proxy

seller :: Proxy "seller"
seller = Proxy

flee :: Proxy "flee"
flee = Proxy

{--select :: (Show a, Read a, KnownSymbol s, KnownSymbol r1, KnownSymbol r2)
     => (Proxy s, a @ s) 
     -> Proxy r1         
     -> Proxy r2
     -> Choreo m (a @ r1, a @ r2)
     
select (s, a) r1 r2 = do
  x <- (s, a) ~> r1
  y <- (s, a) ~> r2
  return (x, y)
  --}

-- | `bookseller` is a choreography that implements the bookseller protocol.
bookseller :: Choreo IO (Maybe Day @ "buyer")
bookseller = do
  title <-
    buyer `locally` \_ -> do
      putStrLn "Enter the title of the book to BUY::"
      getLine

  title' <- (buyer, title) ~> seller
  title'' <- (buyer, title) ~> flee
 
  price <- 
    seller `locally` \_ -> do
      putStrLn "Enter the SELLER price::"
      readLn

  fleePrice <- 
    flee `locally` \_ -> do
      putStrLn "Enter the FLEE price::"
      readLn

  --price' <- (seller, price) ~> buyer
  --fleeprice' <- (flee, fleePrice) ~> buyer
  
  price' <- sel (seller, price) (flee, fleePrice) buyer 
  fleeprice' <- sel (flee, fleePrice) (seller, price) buyer 
  
  decision <- buyer `locally` \un -> return $ (un price') <= (un fleeprice') 
  cond (buyer, decision) \case
    True  -> do
      decision2 <- buyer `locally` \un -> return $ (un price') <= budget
      cond (buyer, decision2) \case
        True -> do 
          deliveryDate  <- seller `locally` \un -> return $ deliveryDateOf (un title')
          deliveryDate' <- (seller, deliveryDate) ~> buyer

          buyer `locally` \un -> do
            putStrLn $ "The book will be delivered by SELLER on " ++ show (un deliveryDate')
            return $ Just (un deliveryDate')
        False -> do
          buyer `locally` \_ -> do
            putStrLn "The book's price is out of the budget"
            return Nothing
    False -> do 
      decision3 <- buyer `locally` \un -> return $ (un fleeprice') <= budget 
      cond (buyer, decision3) \case
        True -> do
          deliveryDate  <- flee `locally` \un -> return $ deliveryDateOf (un title'')
          deliveryDate' <- (flee, deliveryDate) ~> buyer

          buyer `locally` \un -> do
            putStrLn $ "The book will be delivered by FLEE on " ++ show (un deliveryDate')
            return $ Just (un deliveryDate')

        False -> do
          buyer `locally` \_ -> do
            putStrLn "The book's price is out of the budget"
            return Nothing
       


budget :: Int
budget = 100

priceOf :: String -> Int
priceOf "Types and Programming Languages" = 80
priceOf "Homotopy Type Theory"            = 120
priceOf "bookx"            = 20
priceOf "booky"            = 50
priceOf "bookz"            = 150

fleepriceOf :: String -> Int
fleepriceOf "Types and Programming Languages" = 50
fleepriceOf "Homotopy Type Theory"            = 60
fleepriceOf "bookx"            = 40
fleepriceOf "booky"            = 30
fleepriceOf "bookz"            = 130

deliveryDateOf :: String -> Day
deliveryDateOf "Types and Programming Languages" = fromGregorian 2022 12 19
deliveryDateOf "Homotopy Type Theory"            = fromGregorian 2023 01 01
deliveryDateOf "bookx"            = fromGregorian 2009 09 09
deliveryDateOf "booky"            = fromGregorian 2003 03 03
deliveryDateOf "bookz"            = fromGregorian 2025 05 31

main :: IO ()
main = do
  [loc] <- getArgs
  case loc of
    "buyer"  -> runChoreography cfg bookseller "buyer"
    "seller" -> runChoreography cfg bookseller "seller"
    "flee" -> runChoreography cfg bookseller "flee"
  return ()
  where
    cfg = mkHttpConfig [ ("buyer",  ("localhost", 4240))
                       , ("seller", ("localhost", 4341))
                       , ("flee", ("localhost", 4342))
                       ]
