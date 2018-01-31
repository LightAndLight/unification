{-# language RankNTypes #-}
{-# language FlexibleInstances #-}

-- | @lens@'s 'Plated' class lifted to unary type constructors.
--
-- These combinators behave exactly the same as their unlifted counterparts
--
-- See [Control.Lens.Plated](https://hackage.haskell.org/package/lens/docs/Control-Lens-Plated.html) for documentation
module Control.Lens.Plated1 where

import Control.Applicative
import Control.Lens

class Plated1 f where
  plate1 :: Traversal' (f a) (f a)

children1 :: Plated1 f => f a -> [f a]
children1 = toListOf plate1
{-# INLINE children1 #-}

rewrite1 :: Plated1 f => (f a -> Maybe (f a)) -> f a -> f a
rewrite1 = rewriteOf plate1
{-# INLINE rewrite1 #-}

rewriteOn1 :: Plated1 f => ASetter s t (f a) (f a) -> (f a -> Maybe (f a)) -> s -> t
rewriteOn1 b = over b . rewrite1
{-# INLINE rewriteOn1 #-}

rewriteOnOf :: ASetter s t a b -> ASetter a b a b -> (b -> Maybe a) -> s -> t
rewriteOnOf b l = over b . rewriteOf l
{-# INLINE rewriteOnOf #-}

rewriteM1 :: (Monad m, Plated1 f) => (f a -> m (Maybe (f a))) -> f a -> m (f a)
rewriteM1 = rewriteMOf plate1
{-# INLINE rewriteM1 #-}

rewriteMOn1 :: (Monad m, Plated1 f) => LensLike (WrappedMonad m) s t (f a) (f a) -> (f a -> m (Maybe (f a))) -> s -> m t
rewriteMOn1 b = mapMOf b . rewriteM1
{-# INLINE rewriteMOn1 #-}

universe1 :: Plated1 f => f a -> [f a]
universe1 = universeOf plate1
{-# INLINE universe1 #-}

universeOn1 ::  Plated1 f => Getting [f a] s (f a) -> s -> [f a]
universeOn1 b = universeOnOf b plate1
{-# INLINE universeOn1 #-}

cosmos1 :: Plated1 f => Fold (f a) (f a)
cosmos1 = cosmosOf plate1
{-# INLINE cosmos1 #-}

transform1 :: Plated1 f => (f a -> f a) -> f a -> f a
transform1 = transformOf plate1
{-# INLINE transform1 #-}

transformOn1 :: Plated1 f => ASetter s t (f a) (f a) -> (f a -> f a) -> s -> t
transformOn1 b = over b . transform1
{-# INLINE transformOn1 #-}

transformM1 :: (Monad m, Plated1 f) => (f a -> m (f a)) -> f a -> m (f a)
transformM1 = transformMOf plate1
{-# INLINE transformM1 #-}

transformMOn1 :: (Monad m, Plated1 f) => LensLike (WrappedMonad m) s t (f a) (f a) -> (f a -> m (f a)) -> s -> m t
transformMOn1 b = mapMOf b . transformM1
{-# INLINE transformMOn1 #-}
