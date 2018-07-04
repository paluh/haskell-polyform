{-# LANGUAGE DeriveFunctor #-}
module Polyform.Reporter where

import Data.Monoid ((<>))
import Control.Category
import Prelude hiding ((.))

data Report r a = Invalid r | Valid r a
  deriving Functor

instance Monoid r => Applicative (Report r) where
  pure = Valid mempty
  (<*>) (Valid r1 f) (Valid r2 a) = Valid (r1 <> r2) (f a)
  (<*>) (Valid r1 _) (Invalid r2) = Invalid (r1 <> r2)
  (<*>) (Invalid r1) (Invalid r2) = Invalid (r1 <> r2)
  (<*>) (Invalid r1) (Valid r2 _) = Invalid (r1 <> r2)

newtype Reporter m r i o = Reporter (i -> m (Report r o))
  deriving Functor

instance (Monad m, Monoid r) => Applicative (Reporter m r i) where
  pure = Reporter . pure . pure . pure
  (Reporter f) <*> (Reporter a) = Reporter $ \i -> do
    f' <- f i
    a' <- a i
    pure (f' <*> a')

instance (Monoid r, Monad m) => Category (Reporter m r) where
  id = Reporter (pure . pure)
  (Reporter b2c) . (Reporter a2b) = Reporter $ \a -> do
    b <- a2b a
    case b of
      Valid r1 b' -> do
        c <- b2c b'
        pure $ case c of
          Valid r2 c' -> Valid (r1 <> r2) c'
          Invalid r2 -> Invalid (r1 <> r2)
      Invalid r -> pure (Invalid r)
