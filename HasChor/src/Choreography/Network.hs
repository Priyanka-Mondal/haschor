-- | This module defines the `Network` monad, which represents programs run on
-- individual nodes in a distributed system with explicit sends and receives.
-- To run a `Network` program, we provide a `runNetwork` function that supports
-- multiple message transport backends.
module Choreography.Network where

import Choreography.Location
import Control.Monad.Freer
import Control.Monad.IO.Class
import Control.Applicative (Alternative(..),(<|>))
-- * The Network monad

-- | Effect signature for the `Network` monad.
data NetworkSig m a where
  -- | Local computation.
  Run :: m a
      -> NetworkSig m a
  Send :: Show a
       => a
       -> LocTm
       -> NetworkSig m ()
  MaySend :: Show a
       => a
       -> LocTm
       -> NetworkSig m ()
  Recv :: Read a
       => LocTm
       -> NetworkSig m a
  MayRecv1 :: (Read a,Show a)
       => a 
       -> LocTm
       -> NetworkSig m a
  MayRecv2 :: (Read a,Show a)
       => LocTm
       -> a
       -> NetworkSig m a
  PairRecv :: Read a 
       => LocTm
       -> LocTm
       -> NetworkSig m a
  SameSel :: Read a 
       => a
       -> a
       -> NetworkSig m a
  RecvCompare :: (Read a, Eq a )
       => LocTm
       -> LocTm
       -> NetworkSig m a
  BCast :: Show a
        => a
        -> NetworkSig m ()

-- | Monad that represents network programs.
type Network m = Freer (NetworkSig m)

-- * Network operations

-- | Perform a local computation.
run :: m a -> Network m a
run m = toFreer $ Run m

-- | Send a message to a receiver.
send :: Show a => a -> LocTm -> Network m ()
send a l = toFreer $ Send a l

maysend :: Show a => a -> LocTm -> Network m ()
maysend a l = toFreer $ MaySend a l

-- | Receive a message from a sender.
recv :: Read a => LocTm -> Network m a
recv l = toFreer $ Recv l

mayrecv2 :: (Read a, Show a) => LocTm -> a -> Network m a 
mayrecv2 l a =  toFreer $ MayRecv2 l a

mayrecv1 :: (Read a, Show a) => a -> LocTm -> Network m a 
mayrecv1 a l =  toFreer $ MayRecv1 a l

pairrecv :: Read a => LocTm -> LocTm -> Network m a
pairrecv l1 l2 = toFreer $ PairRecv l1 l2

sameSel :: Read a => a -> a -> Network m a
sameSel l1 l2 = toFreer $ SameSel l1 l2

recvCompare :: (Read a, Eq a) => LocTm -> LocTm -> Network m a
recvCompare l1 l2 = toFreer $ RecvCompare l1 l2

-- | Broadcast a message to all participants.
broadcast :: Show a => a -> Network m ()
broadcast a = toFreer $ BCast a

--tryRead :: Read a => LocTm -> Network m a
--tryRead l = toFreer $ TryRecv l

-- * Message transport backends

-- | A message transport backend defines a /configuration/ of type @c@ that
-- carries necessary bookkeeping information, then defines @c@ as an instance
-- of `Backend` and provides a `runNetwork` function.
class Backend c where
  runNetwork :: MonadIO m => c -> LocTm -> Network m a -> m a

{--
TryRecv :: Read a
       => LocTm
       -> NetworkSig m a
--}
