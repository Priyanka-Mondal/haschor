{-# LANGUAGE ExplicitNamespaces #-}

-- | This module defines the interface to HasChor. The client of the library is
-- highly recommended to only use constructs exported by this module.
module Choreography (
  -- * Locations and Located Values
  LocTm,
  LocTy,
  type (@),
  mkLoc,

  -- * The Choreo monad
  Choreo,
  -- ** Choreo operations
  locally,
  (~>),
  (~~>),
  sel,
  com,
  cond,
  cond',
  Host,
  Port,
  HttpConfig,
  mkHttpConfig,
  mkChanMaps,
  runChoreo,
  runChoreography
  ) where

import Choreography.Location ( LocTm, mkLoc, type (@), LocTy )
import Choreography.Choreo
import Choreography.Network
import Choreography.Network.Http
import Choreography.Network.Local
import Control.Monad.IO.Class
import Control.Concurrent.STM
import Data.Proxy



-- | Run a choreography with a message transport backend.
runChoreography :: (Backend config, MonadIO m) => config -> IO ChanMap -> Choreo m a -> LocTm -> m a
runChoreography cfg chanmap choreo l = runNetwork cfg chanmap l (epp choreo l)