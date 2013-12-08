{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

module Data.Region.IORegionRef (
    IORegionRef, newIORegionRef,

    -- *Operations
    readIORegionRef, writeIORegionRef, modifyIORegionRef,

    -- *Atomics
    atomicWriteIORegionRef, atomicModifyIORegionRef
) where

import Control.Monad.Region
import Data.Region.IORefCount

import Control.Monad (liftM)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.IORef (IORef, atomicWriteIORef, atomicModifyIORef, modifyIORef, newIORef, readIORef, writeIORef)

data IORegionRef a m s = IORegionRef (IORef a) (IORefCount m s)

newIORegionRef :: MonadIO m => a -> (a -> m ()) -> RegionT s m (IORegionRef a m s)
newIORegionRef value finalize = do
    ref <- liftIO $ newIORef value
    count <- newIORefCount $ do
        value' <- liftIO $ readIORef ref
        finalize value'
    return $ IORegionRef ref count

readIORegionRef :: MonadIO m => IORegionRef a m (Scope p' c) -> RegionT (Scope p c) m a
readIORegionRef ref = withRef (const . readIORef) ref ()

writeIORegionRef :: MonadIO m => IORegionRef a m (Scope p' c) -> a -> RegionT (Scope p c) m ()
writeIORegionRef = withRef writeIORef

modifyIORegionRef :: MonadIO m => IORegionRef a m (Scope p' c) -> (a -> a) -> RegionT (Scope p c) m ()
modifyIORegionRef = withRef modifyIORef

atomicWriteIORegionRef :: MonadIO m => IORegionRef a m (Scope p' c) -> a -> RegionT (Scope p c) m ()
atomicWriteIORegionRef = withRef atomicWriteIORef

atomicModifyIORegionRef :: MonadIO m => IORegionRef a m (Scope p' c) -> (a -> (a, b)) -> RegionT (Scope p c) m b
atomicModifyIORegionRef = withRef atomicModifyIORef

withRef :: MonadIO m => (IORef a -> b -> IO d) -> IORegionRef a m (Scope p' c) -> b -> RegionT (Scope p c) m d
withRef f (IORegionRef r c) x = withIORefCount c $ liftIO $ f r x

instance Monad m => Resource m (IORegionRef a m) where
    capture (IORegionRef r c) = IORegionRef r `liftM` capture c
    escape (IORegionRef r c) = IORegionRef r `liftM` escape c
