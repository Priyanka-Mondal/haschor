{-# LANGUAGE GADTs              #-}
{-# LANGUAGE ImpredicativeTypes #-}

-- | This module defines `Choreo`, the monad for writing choreographies.
module Choreography.Choreo where

import Choreography.Location
import Choreography.Network
import Control.Monad.Freer
import Data.List
import Data.Proxy
import GHC.TypeLits
import Choreography.Network.Http (checkAndRead)

-- * The Choreo monad

-- | A constrained version of `unwrap` that only unwraps values located at a
-- specific location.
type Unwrap l = forall a. a @ l -> a

-- | Effect signature for the `Choreo` monad. @m@ is a monad that represents
-- local computations.
-- (Comm l a l')
data ChoreoSig m a where
  Local :: (KnownSymbol l)
        => Proxy l
        -> (Unwrap l -> m a)
        -> ChoreoSig m (a @ l)

  Comm :: (Show a, Read a, KnownSymbol l, KnownSymbol l')
       => Proxy l
       -> a @ l
       -> Proxy l'
       -> ChoreoSig m (a @ l')

  Select :: (Show a, Read a, KnownSymbol s1, KnownSymbol s2, KnownSymbol r)
       => Proxy s1
       -> Proxy s2
       -> Proxy r
       -> a @ s1
       -> a @ s2
       -> ChoreoSig m (a @ r)

  Compare :: (Show a, Read a, Eq a, KnownSymbol s1, KnownSymbol s2, KnownSymbol r)
       => Proxy s1
       -> Proxy s2
       -> Proxy r
       -> a @ s1
       -> a @ s2
       -> String
       -> ChoreoSig m (a @ r)

  Cond :: (Show a, Read a, KnownSymbol l)
       => Proxy l
       -> a @ l
       -> (a -> Choreo m b)
       -> ChoreoSig m b
  

-- | Monad for writing choreographies.
type Choreo m = Freer (ChoreoSig m)

-- | Run a `Choreo` monad directly.
runChoreo :: Monad m => Choreo m a -> m a
runChoreo = interpFreer handler
  where
    handler :: Monad m => ChoreoSig m a -> m a
    handler (Local _ m)  = wrap <$> m unwrap
    handler (Comm _ a _) = return $ (wrap . unwrap) a
    handler (Cond _ a c) = runChoreo $ c (unwrap a)

-- |toLocTm : Convert a type-level location to a term-level location.
-- | Endpoint projection.
epp :: Choreo m a -> LocTm -> Network m a
epp c l' = interpFreer handler c
  where
    handler :: ChoreoSig m a -> Network m a
    handler (Local l m)
      | toLocTm l == l' = wrap <$> run (m unwrap)
      | otherwise       = return Empty
    handler (Comm s a r)
      | toLocTm s == toLocTm r = return $ wrap (unwrap a)
      | toLocTm s == l'        = send (unwrap a) (toLocTm r) >> return Empty
      | toLocTm r == l'        = wrap <$> recv (toLocTm s)
      | otherwise              = return Empty
    handler (Select s1 s2 r a b)
      | toLocTm s1 == toLocTm r = return $ wrap (unwrap a)
      | toLocTm r == l'         = wrap <$> pairrecv (toLocTm s1) (toLocTm s2)
      | toLocTm s1 == l'        = maysend (unwrap a) (toLocTm r) >> return Empty
      | toLocTm s2 == l'        = maysend (unwrap b) (toLocTm r) >> return Empty
      | otherwise               = return Empty
    handler (Compare s1 s2 r a b def)
      | toLocTm s1 == toLocTm r = return $ wrap (unwrap a)
      | toLocTm r == l'         = wrap <$> recvCompare (toLocTm s1) (toLocTm s2) def
      | toLocTm s1 == l'        = maysend (unwrap a) (toLocTm r) >> return Empty
      | toLocTm s2 == l'        = maysend (unwrap b) (toLocTm r) >> return Empty
      | otherwise               = return Empty
    handler (Cond l a c)
      | toLocTm l == l' = broadcast (unwrap a) >> epp (c (unwrap a)) l'
      | otherwise       = recv (toLocTm l) >>= \x -> epp (c x) l'
   
--recv :: Read a => LocTm -> Network m a
--recv l = toFreer $ Recv l
-- * Choreo operations
-- | Perform a local computation at a given location.
locally :: KnownSymbol l
        => Proxy l           -- ^ Location performing the local computation.
        -> (Unwrap l -> m a) -- ^ The local computation given a constrained
                             -- unwrap funciton.
        -> Choreo m (a @ l)
locally l m = toFreer (Local l m)

-- | Communication between a sender and a receiver.
(~>) :: (Show a, Read a, KnownSymbol l, KnownSymbol l')
     => (Proxy l, a @ l)  -- ^ A pair of a sender's location and a value located
                          -- at the sender
     -> Proxy l'          -- ^ A receiver's location.
     -> Choreo m (a @ l')
(~>) (l, a) l' = toFreer (Comm l a l')

sel :: (Show a, Read a, KnownSymbol s1, KnownSymbol s2, KnownSymbol r)
     => (Proxy s1, a @ s1)  
     -> (Proxy s2, a @ s2)  
     -> Proxy r
     -> Choreo m (a @ r)
sel (s1, p1) (s2, p2) r = toFreer (Select s1 s2 r p1 p2)

com :: (Show a, Read a, Eq a, KnownSymbol s1, KnownSymbol s2, KnownSymbol r)
     => (Proxy s1, a @ s1)  
     -> (Proxy s2, a @ s2)  
     -> Proxy r
     -> String
     -> Choreo m (a @ r)
com (s1, p1) (s2, p2) r def = toFreer (Compare s1 s2 r p1 p2 def)


-- | Conditionally execute choreographies based on a located value.
cond :: (Show a, Read a, KnownSymbol l)
     => (Proxy l, a @ l)  -- ^ A pair of a location and a scrutinee located on
                          -- it.
     -> (a -> Choreo m b) -- ^ A function that describes the follow-up
                          -- choreographies based on the value of scrutinee.
     -> Choreo m b
cond (l, a) c = toFreer (Cond l a c)


-- | A variant of `~>` that sends the result of a local computation.
(~~>) :: (Show a, Read a, KnownSymbol l, KnownSymbol l')
      => (Proxy l, Unwrap l -> m a) -- ^ A pair of a sender's location and a local
                                    -- computation.
      -> Proxy l'                   -- ^ A receiver's location.
      -> Choreo m (a @ l')
(~~>) (l, m) l' = do
  x <- l `locally` m
  (l, x) ~> l'

-- | A variant of `cond` that conditonally executes choregraphies based on the
-- result of a local computation.
cond' :: (Show a, Read a, KnownSymbol l)
      => (Proxy l, Unwrap l -> m a) -- ^ A pair of a location and a local
                                    -- computation.
      -> (a -> Choreo m b)          -- ^ A function that describes the follow-up
                                    -- choreographies based on the result of the
                                    -- local computation.
      -> Choreo m b
cond' (l, m) c = do
  x <- l `locally` m
  cond (l, x) c


{--
 handler (Cont s a r c)
      | toLocTm s == toLocTm r = return $ wrap (unwrap a)
      | toLocTm s == l' = send (unwrap a) (toLocTm r) >> epp (c (unwrap a)) l'
      | toLocTm r == l' = tryRead (toLocTm r) >>= \x -> epp (c x) l'
      | otherwise               = return Empty

--}

{--
 Cont :: (Show a, Read a, KnownSymbol l, KnownSymbol r)
       => Proxy l
       -> a @ l
       -> Proxy r
       -> (a -> Choreo m (a @ r))
       -> ChoreoSig m (a @ r)
--}

{--
cont :: (Show a, Read a, KnownSymbol l, KnownSymbol l')
     => (Proxy l, a @ l) 
     -> Proxy l' 
     -> (a -> Choreo m (a @ l')) 
     -> Choreo m (a @ l')
cont (l, a) l' c = toFreer (Cont l a l' c)

--}