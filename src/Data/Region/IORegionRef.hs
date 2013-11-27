module Data.Region.IORegionRef (
    IORegionRef, newIORegionRef,

    -- *Operations
    readIORegionRef, writeIORegionRef, modifyIORegionRef,

    -- *Atomics
    atomicWriteIORegionRef, atomicModifyIORegionRef
) where

import Control.Monad.Trans.Region
import Data.Region.IORefCount

import Control.Monad (liftM)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.IORef (IORef, atomicWriteIORef, atomicModifyIORef, modifyIORef, newIORef, readIORef, writeIORef)

data IORegionRef a t c m = IORegionRef (IORef a) (IORefCount t c m)

newIORegionRef :: MonadIO m => a -> (a -> m ()) -> RegionT t c m (IORegionRef a t c m)
newIORegionRef value finalize = do
    ref <- liftIO $ newIORef value
    count <- newIORefCount $ do
        value' <- liftIO $ readIORef ref
        finalize value'
    return $ IORegionRef ref count

readIORegionRef :: MonadIO m => IORegionRef a t' c m -> RegionT t c m a
readIORegionRef ref = withRef (const . readIORef) ref ()

writeIORegionRef :: MonadIO m => IORegionRef a t' c m -> a -> RegionT t c m ()
writeIORegionRef = withRef writeIORef

modifyIORegionRef :: MonadIO m => IORegionRef a t' c m -> (a -> a) -> RegionT t c m ()
modifyIORegionRef = withRef modifyIORef

atomicWriteIORegionRef :: MonadIO m => IORegionRef a t' c m -> a -> RegionT t c m ()
atomicWriteIORegionRef = withRef atomicWriteIORef

atomicModifyIORegionRef :: MonadIO m => IORegionRef a t' c m -> (a -> (a, b)) -> RegionT t c m b
atomicModifyIORegionRef = withRef atomicModifyIORef

withRef :: MonadIO m => (IORef a -> b -> IO d) -> IORegionRef a t' c m -> b -> RegionT t c m d
withRef f (IORegionRef r c) x = withIORefCount c $ liftIO $ f r x

instance Resource (IORegionRef a) where
    capture (IORegionRef r c) = IORegionRef r `liftM` capture c
    escape (IORegionRef r c) = IORegionRef r `liftM` escape c
