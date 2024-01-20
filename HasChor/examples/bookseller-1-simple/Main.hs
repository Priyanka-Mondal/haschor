{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE LambdaCase     #-}

module Main where

import Choreography
import Data.Proxy
import Data.Time
import System.Environment
import Text.Read (Lexeme(String))

buyer :: Proxy "buyer"
buyer = Proxy

seller :: Proxy "seller"
seller = Proxy

flea :: Proxy "flea"
flea = Proxy

seller2 :: Proxy "seller2"
seller2 = Proxy

tcb :: Proxy "tcb"
tcb = Proxy


-- | `bookseller` is a choreography that implements the bookseller protocol.
bookseller :: Choreo IO () --(Maybe Day @ "buyer")
bookseller = do
  title <-
    buyer `locally` \_ -> do
      putStrLn "Enter the title of the book to BUY::"
      getLine
  
  title' <- (buyer, title) ~> seller
  title'' <- (buyer, title) ~> flea
  --title2 <- (buyer, title) ~> seller2
  
  bye <-
    buyer `locally` \_ -> do
      putStrLn "Enter goodbye text for The END"
      getLine

  price <- 
    seller `locally` \_ -> do
      putStrLn "Enter the SELLER price::"
      readLn

  fleaPrice <- 
    flea `locally` \_ -> do
      putStrLn "Enter the flea price::"
      readLn :: IO Int
  
  --price2 <- 
   -- seller2 `locally` \_ -> do
   --   putStrLn "Enter the seller2 price::"
   --   readLn :: IO Int

  --price'' <- com (seller, price) (seller2, price2) buyer
  
  --price'' <- (seller, price) ~> buyer

  --buyer `locally` \un -> do
  --          putStrLn $ "The compare is:: " ++ show (un price'')

  price' <- sel (flea, fleaPrice) (seller, price) buyer

  buyer `locally` \un -> do
            putStrLn $ "The select is:: " ++ show (un price')

  price'' <- sel (flea, fleaPrice) (buyer, price') buyer

  buyer `locally` \un -> do
            putStrLn $ "The second select is:: " ++ show (un price'')

  price' <- sel (seller, price) (flea, fleaPrice) buyer

  buyer `locally` \un -> do
            putStrLn $ "The third select is:: " ++ show (un price')

  price' <- sel (buyer, price'') (buyer, price') buyer

  buyer `locally` \un -> do
            putStrLn $ "The final price is:: " ++ show (un price')

  decision <- buyer `locally` \un -> return $ un price' < budget

  cond (buyer, decision) \case
    True  -> do
               buyer `locally` \un -> do
                                       putStrLn $ "The book will be delivered on " ++ show (deliveryDateOf (un title))
                                       return $ Just (deliveryDateOf (un title))                             
    False -> do
      buyer `locally` \_ -> do
        putStrLn "The book's price is out of the budget"
        return Nothing

  bye' <- (buyer, bye) ~> seller
  bye'' <- (buyer, bye) ~> flea
 -- bye2 <- (buyer, bye) ~> seller2
  --byetcb <- (buyer, bye) ~> tcb
 
  seller `locally` \un -> do
            print (un bye')

  --seller2 `locally` \un -> do
   --         print (un bye2)

  flea `locally` \un -> do
            print (un bye'')

 -- tcb `locally` \un -> do
  --          print (un byetcb)

  return ()
  

budget :: Int
budget = 100

defInt :: Int
defInt = 0

defStr :: String
defStr = "fail"
  

priceOf :: String -> Int
priceOf "Types and Programming Languages" = 80
priceOf "Homotopy Type Theory"            = 120
priceOf "bookx"            = 20
priceOf "booky"            = 50
priceOf "bookz"            = 150

fleapriceOf :: String -> Int
fleapriceOf "Types and Programming Languages" = 50
fleapriceOf "Homotopy Type Theory"            = 60
fleapriceOf "bookx"            = 40
fleapriceOf "booky"            = 30
fleapriceOf "bookz"            = 130

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
    "flea" -> runChoreography cfg bookseller "flea"
  return ()
  where
    cfg = mkHttpConfig [ ("buyer",  ("localhost", 4240))
                       , ("seller", ("localhost", 4341))
                       , ("flea", ("localhost", 4342))
                       ]


{--
hi <- 
    buyer `locally` \_ -> do
      putStrLn "Say Hi"
      readLn :: IO Int

--}                       