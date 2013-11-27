module Data.Region.IORefCount (
    IORefCount, newIORefCount
) where

import Control.Monad.Region

import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.IORef (newIORef, atomicModifyIORef')

-- |Maintains a reference count, and invokes a finalizer when the reference
-- count hits zero.
newtype IORefCount c m = IORefCount (Handle c m)

-- |@newIORefCount f@ creates a new `IORefCount` with an initial reference count
-- of 1. The finalizer @f@ is run when the reference count hits zero.
newIORefCount :: MonadIO m => m () -> RegionT t c m (IORefCount c m)
newIORefCount finalize = do
    -- The reference is automatically incremented when newHandle is called.
    ref <- liftIO $ newIORef (0 :: Integer)
    handle <- newHandle (acquire ref) (release ref)
    return $ IORefCount handle
  where
    acquire ref = liftIO $ atomicModifyIORef' ref (\x -> (x + 1, ()))
    release ref = do
        count <- liftIO $ atomicModifyIORef' ref (\x -> (x - 1, x))
        when (count == 1) finalize
