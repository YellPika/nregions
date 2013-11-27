{-# LANGUAGE GeneralizedNewtypeDeriving, RankNTypes, Trustworthy #-}

module Control.Monad.Region (
    -- *Region Types
    Master, Slave,

    -- *Regions
    RegionT, runRegionT,

    -- *Scoping and Resetting
    scope, reset,

    -- *Resources
    Resource (..),

    -- *Handles
    Handle, newHandle, withHandle
) where

import Control.Arrow (first, second)
import Control.Applicative (Alternative, Applicative)
import Control.Monad (MonadPlus)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Class (MonadTrans, lift)
import Control.Monad.Trans.State (StateT, evalStateT, get, gets, modify)
import Data.Monoid (Monoid, (<>), mempty, mappend)

data Context m = Context {
    initialize :: m (),
    finalize :: m ()
}

instance Monad m => Monoid (Context m) where
    mempty = Context (return ()) (return ())
    mappend (Context x y) (Context x' y') =
        -- Initializes are executed in order.
        -- Finalizers are executed in reverse.
        Context (x >> x') (y' >> y)

-- |A master region cannot capture resources from the surrounding scope.
data Master

-- |A slave region may capture resources from the scope with the context `c`.
data Slave c

-- |A region is a computation that ensures that any open resources are closed
-- when the computation terminates.
--
-- [@t@] The region's type (`Master` or `Slave`).
--
-- [@c@] The region's context.
--
-- [@m@] The inner monad.
--
-- [@a@] The computation result type.
newtype RegionT t c m a = RegionT (StateT (Context m, Context m) m a)
  deriving (
    Alternative, Applicative,
    Functor, Monad,
    MonadIO, MonadPlus)

instance MonadTrans (RegionT t c) where
    lift = RegionT . lift

-- |Executes a region computation. Any resources that are opened within the
-- region are released when the computation terminates.
runRegionT :: Monad m => (forall c. RegionT Master c m a) -> m a
runRegionT region = fullRun region mempty

-- |/Scoping/ nests a region within another region. The inner region does not
-- inherit any resources from the parent, but may do so explicitly by calling
-- `capture`. Resources may be used by the outer scope by calling `escape`.
scope :: Monad m => (forall c'. RegionT (Slave c) c' m a) -> RegionT t c m a
scope region = RegionT $ do
    (output, escaped) <- lift $ run region mempty
    modify $ first (<> escaped)
    return output

-- |/Resetting/ nests a region within another region. The inner region
-- automatically captures all resources from the outer region. Resources may not
-- escape to the outer scope. During a reset, users can execute arbitrary
-- operations that don't involve resources, such as forking.
reset :: Monad m => (m a -> m b) -> RegionT Master c m a -> RegionT t c m b
reset f region = RegionT $ do
    local <- gets fst
    lift $ do
        initialize local
        f $ fullRun region local

-- Fully evaluates a region and then cleans up any leftover handles. Returns the
-- result as well as a list of escaped handles.
run :: Monad m => RegionT t c m a -> Context m -> m (a, Context m)
run (RegionT region) input = flip evalStateT (input, mempty) $ do
    output <- region
    (local, escaped) <- get
    lift $ finalize local
    return (output, escaped)

-- Like run, but ensures the escaped context is finalized.
fullRun :: Monad m => RegionT t c m a -> Context m -> m a
fullRun region input = do
    (output, escaped) <- run region input
    finalize escaped
    return output

-- |A resource defines operations for changing scopes.
class Resource a where
    -- |Imports a resource from the surrounding scope. A resource can only be
    -- brought in one `scope` level at a time, and may not be transferred across
    -- a `reset`.
    capture :: Monad m => a c m -> RegionT (Slave c) c' m (a c' m)

    -- |Exports a resource into the surrounding scope. A resource can only be
    -- exported out of one `scope` level at time, and may not be transferred
    -- across a `reset`.
    escape :: Monad m => a c m -> RegionT (Slave c') c m (a c' m)

-- |Handles are the simplest possible resource type. Users can specify actions
-- to execute when the containing region is reset or the handle changes scope.
data Handle c m = Handle (Context m)

-- |@newHandle r s@ creates a new `Handle`. @r@ is called immediately, and when
-- `reset` is called within the containing region. @s@ is called when the handle
-- is `capture`d or `escape`d.
newHandle :: Monad m => m () -> m () -> RegionT t c m (Handle c m)
newHandle onReset onScope = newHandleOn first $ Context onReset onScope

-- Creates a new handle from an existing context, ensures it is initialized,
-- and then adds its context to the list of contexts to finalize when the
-- containing region goes out of scope. append specifies which context to append
-- to (local or escaped).
newHandleOn :: Monad m =>
    -- This signature itself is a good reason to use RankNTypes.
    -- (forall a. (a -> a) -> (a, a) -> (a, a)) anybody?
    ((Context m -> Context m) ->
        (Context m, Context m) ->
        (Context m, Context m)) ->
    Context m ->
    RegionT t c m (Handle c' m)
newHandleOn append context = RegionT $ do
    lift $ initialize context
    modify $ append (<> context)
    return $ Handle context

-- |@withHandle@ lifts an operation into the region monad. A handle is specified
-- to ensure that the operation can only be used when the handle is in the same
-- context as the current region.
withHandle :: Monad m => Handle c m -> m a -> RegionT t c m a
withHandle _ = lift

instance Resource Handle where
    capture (Handle context) = newHandleOn first context
    escape (Handle context) = newHandleOn second context
