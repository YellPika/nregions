{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, RankNTypes,
             TupleSections, TypeOperators, UndecidableInstances #-}

module Control.Monad.Generator (
    GeneratorT (..), yield
) where

import Control.Arrow (second)
import Control.Applicative (Alternative, Applicative, (<*>), (<|>), pure, empty)
import Control.Monad (MonadPlus, ap, liftM, mzero, mplus)
import Control.Monad.Fix (MonadFix, mfix)
import Control.Monad.Trans (MonadIO, MonadTrans, lift, liftIO)
import Control.Monad.Error (MonadError, catchError, throwError)
import Control.Monad.Reader (MonadReader, ask, local)
import Control.Monad.State (MonadState, get, put)

newtype GeneratorT y m a = GeneratorT {
    runGeneratorT :: m (Either a (y, GeneratorT y m a))
}

yield :: Monad m => y -> GeneratorT y m ()
yield = GeneratorT . return . Right . (, return ())

instance Monad m => Functor (GeneratorT y m) where
    fmap = liftM

instance Monad m => Applicative (GeneratorT y m) where
    pure = return
    (<*>) = ap

instance MonadPlus m => Alternative (GeneratorT y m) where
    empty = mzero 
    (<|>) = mplus

instance Monad m => Monad (GeneratorT y m) where
    return = GeneratorT . return . Left
    x >>= f = GeneratorT $
        runGeneratorT x >>=
        either (runGeneratorT . f)
               (return . Right . second (>>= f))

instance MonadIO m => MonadIO (GeneratorT y m) where
    liftIO = lift . liftIO

instance MonadFix m => MonadFix (GeneratorT y m) where
    mfix f = GeneratorT $ mfix mfix'
      where
        mfix' (Left x) = runGeneratorT (f x)
        mfix' (Right (y, n)) = return $
            Right (y, GeneratorT (runGeneratorT n >>= mfix'))

instance MonadPlus m => MonadPlus (GeneratorT y m) where
    mzero = GeneratorT mzero
    mplus x y = GeneratorT $
        liftM (fmap $ second $ mplus y) (runGeneratorT x)
        `mplus`
        liftM (fmap $ second $ mplus x) (runGeneratorT y)

instance MonadTrans (GeneratorT y) where
    lift = GeneratorT . liftM Left

instance MonadError e m => MonadError e (GeneratorT y m) where
    throwError = lift . throwError
    catchError x f = GeneratorT $
        liftM (either Left (Right . second (`catchError` f))) $
        catchError (runGeneratorT x) (runGeneratorT . f)

instance MonadReader r m => MonadReader r (GeneratorT y m) where
    ask = lift ask
    local f x = GeneratorT $
        liftM (either Left (Right . second (local f))) $
        local f (runGeneratorT x)

instance MonadState s m => MonadState s (GeneratorT y m) where
    get = lift get
    put = lift . put
