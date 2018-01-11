{-# language DefaultSignatures #-}
{-# language DeriveFunctor #-}
{-# language DeriveFoldable #-}
{-# language DeriveTraversable #-}
{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language FunctionalDependencies #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language LambdaCase #-}
{-# language MultiParamTypeClasses #-}
{-# language MultiParamTypeClasses #-}
{-# language StandaloneDeriving #-}
{-# language RankNTypes #-}
{-# language UndecidableInstances #-}
module Control.Monad.Unify
  ( -- * Monad
    UnifyT
  , runUnifyT
  , UVar
    -- * Operations
  , fresh
  , occurs
  , union
  , find
  , unify
  , freeze
    -- * Type classes
  , Unifiable(..)
  , AsVar(..)
  , AsUnificationError(..)
  , UnificationError(..)
  , HasAnnotation(..)
  )
where

import Control.Lens.Plated1

import Control.Lens
import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Writer
import Data.Functor.Classes
import Data.Equivalence.Monad
import Data.Foldable hiding (find)
import Data.Traversable

-- | Unification variables. Use 'fresh' to obtain new 'UVar's
newtype UVar = UVar Int deriving (Eq, Show, Ord)

-- | Terms that can be unified
class Unifiable term where
  -- |
  -- Checks for top-level equality, for example:
  --
  -- @
  -- data Ty = TyArr Ty Ty | TyVar String
  -- instance Unifiable Ty where
  --   toplevelEqual TyArr{} TyArr{} = True
  --   toplevelEqual (TyVar a) (TyVar b) = a == b
  --   toplevelEqual _ _ = False
  -- @
  --
  -- Must obey the law
  -- @forall t u. toplevelEqual t u ==> lengthOf plated t == lengthOf plated u@
  toplevelEqual :: Eq a => term a -> term a -> Bool

-- | Terms from which 'variables' can be extracted
class AsVar term where
  _Var :: Prism' (term var) var

-- | Datatypes which can contain unification errors.
class AsUnificationError e term ann | e -> term, e -> ann where
  _OccursError :: Prism' e (UVar, term, ann)
  _MismatchError :: Prism' e (term, term, ann)

-- | Concrete unification error datatype
data UnificationError term ann
  = OccursError UVar term ann
  | MismatchError term term ann
  deriving (Eq, Show)

instance AsUnificationError (UnificationError (term a) ann) (term a) ann where
  _OccursError =
    prism'
      (\(a, b, c) -> OccursError a b c)
      (\case
          OccursError a b c -> Just (a, b, c)
          _ -> Nothing)

  _MismatchError =
    prism'
      (\(a, b, c) -> MismatchError a b c)
      (\case
          MismatchError a b c -> Just (a, b, c)
          _ -> Nothing)

-- | Terms from which 'annotations' can be extracted
class HasAnnotation term ann | term -> ann where
  annotation :: Lens' (term a) ann

  default annotation :: Lens' (term a) ()
  annotation = lens (const ()) const

newtype E a b c d = E { getE :: forall s. EquivT s a b c d }

instance Functor c => Functor (E a b c) where
  fmap f (E a) = E $ fmap f a

instance (Applicative c, Monad c) => Applicative (E a b c) where
  pure a = E $ pure a
  E f <*> E a = E $ f <*> a

instance Monad c => Monad (E a b c) where
  E a >>= f = E $ a >>= (getE . f)

newtype UnifyT t m a = UnifyT { runUnifyT' :: StateT Int (E t t m) a }

instance Functor m => Functor (UnifyT t m) where
  fmap f (UnifyT a) = UnifyT $ fmap f a

instance (Applicative m, Monad m) => Applicative (UnifyT t m) where
  pure a = UnifyT $ pure a
  UnifyT f <*> UnifyT a = UnifyT $ f <*> a

instance Monad m => Monad (UnifyT t m) where
  UnifyT a >>= f = UnifyT $ a >>= (runUnifyT' . f)

instance MonadState s m => MonadState s (UnifyT t m) where
  get = UnifyT . lift $ E get
  put a = UnifyT . lift $ E (put a)

instance MonadError e m => MonadError e (UnifyT t m) where
  catchError a b =
    UnifyT $ StateT $ \s -> E ((,) <$> catchError (getE . flip evalStateT s $ runUnifyT' a) (getE . flip evalStateT s . runUnifyT' . b) <*> pure s)
  throwError a = UnifyT . lift $ E (throwError a)

instance MonadWriter w m => MonadWriter w (UnifyT t m) where
  writer a = UnifyT . lift $ E (writer a)
  tell a = UnifyT . lift $ E (tell a)
  listen a =
    UnifyT $ StateT $ \s -> E ((,) <$> listen (getE . flip evalStateT s $ runUnifyT' a) <*> pure s)
  pass a =
    UnifyT $ StateT $ \s -> E ((,) <$> pass (getE . flip evalStateT s $ runUnifyT' a) <*> pure s)

instance MonadReader r m => MonadReader r (UnifyT t m) where
  ask = UnifyT . lift $ E ask
  local a b =
    UnifyT $ StateT $ \s -> E ((,) <$> local a (getE . flip evalStateT s $ runUnifyT' b) <*> pure s)

-- | Generate an fresh 'UVar'
fresh :: Monad m => UnifyT t m UVar
fresh = UnifyT $ do
  count <- get
  put $! count+1
  pure $ UVar count

-- | Check whether or not a 'UVar' is present in a term
occurs :: (AsVar t, Plated1 t) => UVar -> t (Either UVar a) -> Bool
occurs n t = n `elem` (t ^.. cosmos1._Var._Left)

-- | Union the equivalence classes of two terms
union :: (Monad m, Ord t) => t -> t -> UnifyT t m ()
union a b = UnifyT . lift $ E (equate a b)

-- | Find the representative of term's equivalence class
find :: (Plated1 t, Ord (t a), Monad m) => t a -> UnifyT (t a) m (t a)
find a = traverseOf plate1 find =<< (UnifyT $ lift $ E (classDesc a))

-- | Unify two terms
unify
  :: ( AsVar t
     , HasAnnotation t ann
     , Unifiable t
     , Plated1 t
     , Ord (t (Either UVar a))
     , Eq a
     , AsUnificationError e (t (Either UVar a)) ann
     , MonadError e m
     )
  => t (Either UVar a)
  -> t (Either UVar a)
  -> UnifyT (t (Either UVar a)) m ()
unify m n = do
  a <- find m
  b <- find n
  if toplevelEqual a b
    then traverse_ (uncurry unify) (zip (a ^.. plate1) (b ^.. plate1))
    else
      case (a ^? _Var, b ^? _Var) of
        (Just (Left a'), _)
          | occurs a' b -> throwError $ _OccursError # (a', b, a ^. annotation)
          | otherwise -> union a b
        (_, Just (Left b'))
          | occurs b' a -> throwError $ _OccursError # (b', a, a ^. annotation)
          | otherwise -> union a b
        (_, _) -> unless (a == b) . throwError $ _MismatchError # (a, b, a ^. annotation)

runUnifyT :: (AsVar t, Monad m, Plated1 t) => UnifyT (t (Either UVar a)) m res -> m res
runUnifyT a = runEquivT id combine (getE . flip evalStateT 0 $ runUnifyT' a)
  where
    combine u v =
      case u ^? _Var._Left of
        Just{} -> v
        Nothing -> case v ^? _Var._Left of
          Just{} -> u
          Nothing
            | lengthOf (plate1._Var._Left) u < lengthOf (plate1._Var._Left) v -> u
            | otherwise -> v

freeze
  :: ( AsVar t 
     , Monad m
     , Ord (t (Either UVar a))
     , Plated1 t
     , Traversable t
     , Monad t
     )
  => t (Either UVar a)
  -> UnifyT (t (Either UVar a)) m (Maybe (t a))
freeze = fmap (fmap join . sequence) . go
  where
    go t =
      for t $ \x -> case x of
        Right a -> pure . Just $ _Var # a
        _ -> do
          r <- find $ _Var # x
          case r ^.. cosmos1._Var._Left of
            [] -> freeze r
            _ -> pure Nothing
