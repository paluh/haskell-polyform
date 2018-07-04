{-# LANGUAGE DeriveFunctor #-}
module Polyform.Validator where

import Data.Monoid ((<>))
import Data.Semigroup (Semigroup)
import Data.Validation
import Control.Category
import Prelude hiding ((.))

newtype Validator m e i o = Validator (i -> m (Validation e o))
  deriving Functor

instance (Monad m, Semigroup e) => Applicative (Validator m e i) where
  pure = Validator . pure . pure . pure
  (Validator f) <*> (Validator a) = Validator $ \i -> do
    f' <- f i
    a' <- a i
    pure (f' <*> a')

instance (Semigroup e, Monad m) => Category (Validator m e) where
  id = Validator (pure . pure)
  (Validator b2c) . (Validator a2b) = Validator $ \a -> do
    b <- a2b a
    case b of
      Success b' -> b2c b'
      Failure e -> pure (Failure e)
