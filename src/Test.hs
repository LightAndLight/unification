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
module Test where

import Control.Applicative
import Control.Lens
import Control.Monad.Except
import Control.Monad.Identity
import Control.Monad.State
import Control.Monad.Trans
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.IO.Class
import Data.Equivalence.Monad
import Data.Foldable hiding (find)
import Data.Functor.Classes
import Data.Monoid

data Data = A | B | C | D deriving (Eq, Ord, Show)

data Ty' a = Ty' a :-> Ty' a | Var a | Bool deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

newtype UVar = UVar Int deriving (Eq, Show, Ord)

class Unifiable t where
  toplevelEqual :: t -> t -> Bool

class AsVar t a | t -> a where
  _Var :: Prism' t a

class AsUnificationError e t a | e -> t, e -> a where
  _OccursError :: Prism' e (UVar, t, a)
  _MismatchError :: Prism' e (t, t, a)

data UnificationTrace t = UnificationTrace t t [UnificationDetails t]
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)
data UnificationDetails t
  = Unifying (UnificationTrace t)
  | Representatives (t, t) (t, t)
  | Success
  | Failure
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

class HasAnnotation t a | t -> a where
  annotation :: Lens' t a

  default annotation :: Lens' t ()
  annotation = lens (const ()) const

newtype E a b c d = E { getE :: forall s. EquivT s a b c d }

instance Functor c => Functor (E a b c) where
  fmap f (E a) = E $ fmap f a

instance (Applicative c, Monad c) => Applicative (E a b c) where
  pure a = E $ pure a
  E f <*> E a = E $ f <*> a

instance Monad c => Monad (E a b c) where
  E a >>= f = E $ a >>= (getE . f)

newtype Unify t m a = Unify { runUnify' :: StateT Int (E t t m) a }

instance Functor m => Functor (Unify t m) where
  fmap f (Unify a) = Unify $ fmap f a

instance (Applicative m, Monad m) => Applicative (Unify t m) where
  pure a = Unify $ pure a
  Unify f <*> Unify a = Unify $ f <*> a

instance Monad m => Monad (Unify t m) where
  Unify a >>= f = Unify $ a >>= (runUnify' . f)

instance MonadState s m => MonadState s (Unify t m) where
  get = Unify . lift $ E get
  put a = Unify . lift $ E (put a)

instance MonadError e m => MonadError e (Unify t m) where
  catchError a b =
    Unify $ StateT $ \s -> E ((,) <$> catchError (getE . flip evalStateT s $ runUnify' a) (getE . flip evalStateT s . runUnify' . b) <*> pure s)
  throwError a = Unify . lift $ E (throwError a)

instance MonadWriter w m => MonadWriter w (Unify t m) where
  writer a = Unify . lift $ E (writer a)
  tell a = Unify . lift $ E (tell a)
  listen a =
    Unify $ StateT $ \s -> E ((,) <$> listen (getE . flip evalStateT s $ runUnify' a) <*> pure s)
  pass a =
    Unify $ StateT $ \s -> E ((,) <$> pass (getE . flip evalStateT s $ runUnify' a) <*> pure s)

instance MonadReader r m => MonadReader r (Unify t m) where
  ask = Unify . lift $ E ask
  local a b =
    Unify $ StateT $ \s -> E ((,) <$> local a (getE . flip evalStateT s $ runUnify' b) <*> pure s)

fresh :: Monad m => Unify t m UVar
fresh = Unify $ do
  count <- get
  put $! count+1
  pure $ UVar count

occurs :: (AsVar t (Either UVar a), Plated t) => UVar -> t -> Bool
occurs n t = n `elem` (t ^.. cosmos._Var._Left)

union :: (Monad m, Ord t) => t -> t -> Unify t m ()
union a b = Unify . lift $ E (equate a b)

find :: (Plated t, Ord t, Monad m) => t -> Unify t m t
find a = traverseOf plate find =<< (Unify $ lift $ E (classDesc a))

unify
  :: ( AsVar t (Either UVar a)
     , HasAnnotation t ann
     , Unifiable t
     , Plated t
     , Ord t
     , AsUnificationError e t ann
     , MonadError e m
     )
  => t
  -> t
  -> Unify t m ()
unify m n = do
  a <- find m
  b <- find n
  if toplevelEqual a b
    then traverse_ (uncurry unify) (zip (a ^.. plate) (b ^.. plate))
    else
      case (a ^? _Var, b ^? _Var) of
        (Just (Left a'), _)
          | occurs a' b -> throwError $ _OccursError # (a', b, a ^. annotation)
          | otherwise -> union a b
        (_, Just (Left b'))
          | occurs b' a -> throwError $ _OccursError # (b', a, a ^. annotation)
          | otherwise -> union a b
        (_, _) -> unless (a == b) . throwError $ _MismatchError # (a, b, a ^. annotation)

runUnify :: (AsVar t (Either UVar v), Monad m, Plated t) => Unify t m res -> m res
runUnify a = runEquivT id combine (getE . flip evalStateT 0 $ runUnify' a)
  where
    combine u v =
      case u ^? _Var._Left of
        Just{} -> v
        Nothing -> case v ^? _Var._Left of
          Just{} -> u
          Nothing
            | lengthOf (plate._Var._Left) u < lengthOf (plate._Var._Left) v -> u
            | otherwise -> v

instance Plated (Ty' a) where
  plate f (a :-> b) = liftA2 (:->) (f a) (f b)
  plate _ a = pure a

instance AsVar (Ty' a) a where
  _Var = prism' Var (\case; Var a -> Just a; _ -> Nothing) 

instance Eq a => Unifiable (Ty' a) where
  toplevelEqual (a :-> b) (c :-> d) = True
  toplevelEqual (Var a) (Var b) = a == b
  toplevelEqual Bool Bool = True
  toplevelEqual _ _ = False

data UnificationError t ann
  = OccursError UVar t ann
  | MismatchError t t ann
  deriving (Eq, Show)

instance AsUnificationError (UnificationError t ann) t ann where
  _OccursError = prism' (\(a, b, c) -> OccursError a b c) (\case; OccursError a b c -> Just (a, b, c); _ -> Nothing)
  _MismatchError = prism' (\(a, b, c) -> MismatchError a b c) (\case; MismatchError a b c -> Just (a, b, c); _ -> Nothing)

instance HasAnnotation (Ty' a) () where

test :: Either (UnificationError (Ty' (Either UVar String)) ()) _
test =
  runUnify $ do
    let one = Var (Left $ UVar 1)
    let two = Var (Left $ UVar 2)
    let three = Var (Left $ UVar 3)
    let a = Var (Right "a")
    let b = Var (Right "b")
    unify one (two :-> Bool)
    unify two (one :-> Bool)
