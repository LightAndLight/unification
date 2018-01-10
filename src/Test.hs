{-# language DeriveFunctor #-}
{-# language DeriveFoldable #-}
{-# language DeriveTraversable #-}
{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language FunctionalDependencies #-}
{-# language LambdaCase #-}
{-# language MultiParamTypeClasses #-}
{-# language MultiParamTypeClasses #-}
{-# language StandaloneDeriving #-}
{-# language RankNTypes #-}
module Test where

import Control.Applicative
import Control.Lens
import Control.Monad.Except
import Control.Monad.Identity
import Control.Monad.Trans
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

class HasVar t a | t -> a where
  _Var :: Prism' t a

class AsUnificationError e t | e -> t where
  _OccursError :: Prism' e (UVar, t)
  _MismatchError :: Prism' e (t, t)

occurs :: (HasVar t (Either UVar a), Plated t) => UVar -> t -> Bool
occurs n t = n `elem` (t ^.. cosmos._Var._Left)

find :: (Plated t, Ord t, AsUnificationError e t, MonadError e m) => t -> EquivT s t t m t
find a = traverseOf plate find =<< classDesc a

unify
  :: ( HasVar t (Either UVar a)
     , Unifiable t
     , Plated t
     , Ord t
     , AsUnificationError e t
     , MonadError e m
     )
  => t
  -> t
  -> EquivT s t t m ()
unify m n = do
  a <- find m
  b <- find n
  if toplevelEqual a b
    then traverse_ (uncurry unify) (zip (a ^.. plate) (b ^.. plate))
    else
      case (a ^? _Var, b ^? _Var) of
        (Just (Left a'), _)
          | occurs a' b -> throwError $ _OccursError # (a', b)
          | otherwise -> equate a b
        (_, Just (Left b'))
          | occurs b' a -> throwError $ _OccursError # (b', a)
          | otherwise -> equate a b
        (_, _) -> unless (a == b) . throwError $ _MismatchError # (a, b)

runUnifier :: (HasVar t (Either UVar v), Monad m, Plated t) => (forall s. EquivT s t t m res) -> m res
runUnifier = runEquivT id combine
  where
    combine u v =
      case u ^? _Var._Left of
        Nothing -> case v ^? _Var._Left of
          Nothing
            | lengthOf (plate._Var._Left) u < lengthOf (plate._Var._Left) v -> u
            | otherwise -> v
          Just{} -> u
        Just{} -> v

instance Plated (Ty' a) where
  plate f (a :-> b) = liftA2 (:->) (f a) (f b)
  plate _ a = pure a

instance HasVar (Ty' a) a where
  _Var = prism' Var (\case; Var a -> Just a; _ -> Nothing) 

instance Eq a => Unifiable (Ty' a) where
  toplevelEqual (a :-> b) (c :-> d) = True
  toplevelEqual (Var a) (Var b) = a == b
  toplevelEqual Bool Bool = True
  toplevelEqual _ _ = False

data UnificationError t
  = OccursError UVar t
  | MismatchError t t

instance AsUnificationError (UnificationError t) t where
  _OccursError = prism' (uncurry OccursError) (\case; OccursError a b -> Just (a, b); _ -> Nothing)
  _MismatchError = prism' (uncurry MismatchError) (\case; MismatchError a b -> Just (a, b); _ -> Nothing)


test :: Either (UnificationError (Ty' (Either UVar String))) _
test =
  runUnifier $ do
    let one = Var (Left $ UVar 1)
    let two = Var (Left $ UVar 2)
    let three = Var (Left $ UVar 3)
    let a = Var (Right "a")
    let b = Var (Right "b")
    unify one (Bool :-> Bool)
    unify two (three :-> Bool)
    unify one two
    find three
