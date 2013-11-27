{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Region.IORefCount (
    IORefCount, newIORefCount, withIORefCount
) where

import Control.Monad.Trans.Region

import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.IORef (newIORef, atomicModifyIORef')

-- |Maintains a reference count, and invokes a finalizer when the reference
-- count hits zero.
newtype IORefCount t c m = IORefCount (Handle t c m)
  deriving Resource

-- |@newIORefCount f@ creates a new `IORefCount` with an initial reference count
-- of 1. The finalizer @f@ is run when the reference count hits zero.
newIORefCount :: MonadIO m => m () -> RegionT t c m (IORefCount t c m)
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

-- |@withIORefCount@ lifts an operation into the region monad. A handle is
-- specified to ensure that the operation can only be used when the handle is
-- in the same context as the current region.
withIORefCount :: MonadIO m => IORefCount t' c m -> m a -> RegionT t c m a
withIORefCount (IORefCount handle) = withHandle handle
