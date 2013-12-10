{-# LANGUAGE FlexibleContexts, FlexibleInstances, FunctionalDependencies,
             GeneralizedNewtypeDeriving, MultiParamTypeClasses, RankNTypes,
             UndecidableInstances #-}

module Control.Monad.Region (
    -- *Regions
    Scope, RegionT, runRegionT,

    -- *Scoping and Resetting
    scope, reset,

    -- *Resources
    Resource (..),

    -- *Handles
    Handle, newHandle, withHandle
) where

import Control.Applicative (Applicative, Alternative)
import Control.Monad ((>=>))
import Control.Monad.Trans (MonadIO, MonadTrans, lift)
import Control.Monad.Error (MonadError, catchError, throwError)
import Control.Monad.Generator (GeneratorT, runGeneratorT, yield)
import Control.Monad.Reader (ReaderT, runReaderT, ask, local)

data Box m = Box { load :: m (), unload :: m () }

data Command m = Capture (Box m) | Escape (Box m)

-- |A region is a computation that ensures that any open resources are closed
-- when the computation terminates.
--
-- [@s@] A `Scope` representing the region's context.
--
-- [@m@] The inner monad.
--
-- [@a@] The computation result type.
newtype RegionT s m a = RegionT {
    unRegionT :: GeneratorT (Command m) (ReaderT [Box m] m) a
} deriving (Alternative, Applicative, Functor, Monad, MonadError e, MonadIO)

instance MonadTrans (RegionT s) where
    lift = RegionT . lift . lift

-- |Describes a region's context.
--
-- [@p@] The parent scope.
--
-- [@c@] The current (child) scope.
data Scope p c

-- |Executes a region computation. Any resources that are opened within the
-- region are released when the computation terminates.
runRegionT :: MonadError e m => (forall p c. RegionT (Scope p c) m a) -> m a
runRegionT region = runMaster region []

-- |/Resetting/ nests a region within another region. The inner region
-- automatically captures all resources from the outer region. Resources may not
-- escape to the outer scope. During a reset, users can execute arbitrary
-- operations that don't involve resources, such as forking.
reset :: MonadError e m => (m a -> m b) -> (forall p'. RegionT (Scope p' c) m a) -> RegionT (Scope p c) m b
reset transform region = RegionT $ do
    boxes <- ask
    lift $ lift $ do
        loadAll boxes
        transform $ runMaster region [] `finally` unloadAll boxes
  where
    unloadAll = foldr (finally . unload) (return ())

    loadAll [] = return ()
    loadAll (x:xs) = do
        load x
        loadAll xs `onError` unload x

runMaster :: MonadError e m => RegionT s m a -> [Box m] -> m a
runMaster = runReaderT . run . unRegionT
  where
    run = runGeneratorT >=> either return command
    command (Escape _, _) = undefined
    command (Capture box, next) = do
        lift (load box)
        local (box:) (run next) `finally` lift (unload box)

-- |/Scoping/ nests a region within another region. The inner region does not
-- inherit any resources from the parent, but may do so explicitly by calling
-- `capture`. Resources may be used by the outer scope by calling `escape`.
scope :: MonadError e m => (forall c'. RegionT (Scope c c') m a) -> RegionT (Scope p c) m a
scope region = RegionT $ local (const []) $ run $ unRegionT region
  where
    run x = lift (runGeneratorT x) >>= either return (uncurry command)
    lift' f = lift . lift . f
    command (Escape box) next = do
        yield $ Capture box
        run next
    command (Capture box) next = do
        lift' load box
        local (box:) (run next) `finally` lift' unload box

finally :: MonadError e m => m a -> m () -> m a
finally execute finalize = do
    output <- execute `onError` finalize
    finalize
    return output

onError :: MonadError e m => m a -> m () -> m a
onError execute handle = execute `catchError` \e -> do
    handle
    throwError e

-- |A resource defines operations for changing scopes.
class Monad m => Resource m r | r -> m where
    -- |Imports a resource from the surrounding scope. A resource can only be
    -- brought in one `scope` level at a time, and may not be transferred across
    -- a `reset`.
    capture :: r (Scope p' p) -> RegionT (Scope p c) m (r (Scope p c))

    -- |Exports a resource into the surrounding scope. A resource can only be
    -- exported out of one `scope` level at time, and may not be transferred
    -- across a `reset`.
    escape :: r (Scope c c') -> RegionT (Scope c c') m (r (Scope p c))

-- |Handles are the simplest possible resource type. Users can specify actions
-- to execute when the containing region is reset or the handle changes scope.
newtype Handle m s = Handle (Box m)

-- |@newHandle r s@ creates a new `Handle`. @r@ is called immediately, and when
-- `reset` is called within the containing region. @s@ is called when the handle
-- is `capture`d or `escape`d.
newHandle :: Monad m => m () -> m () -> RegionT s m (Handle m s)
newHandle acq rel = rescopeHandle Capture $ Handle $ Box acq rel

-- |@withHandle@ lifts an operation into the region monad. A handle is specified
-- to ensure that the operation can only be used when the handle is in the same
-- context as the current region.
withHandle :: Monad m => Handle m (Scope p' c) -> m a -> RegionT (Scope p c) m a
withHandle _ = lift

rescopeHandle :: Monad m => (Box m -> Command m) -> Handle m t -> RegionT s m (Handle m s')
rescopeHandle f (Handle x) = RegionT $ do
    yield $ f x
    return $ Handle x

instance Monad m => Resource m (Handle m) where
    capture = rescopeHandle Capture
    escape = rescopeHandle Escape
