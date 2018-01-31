{-# language DeriveFunctor #-}
{-# language DeriveFoldable #-}
{-# language DeriveTraversable #-}
{-# language MultiParamTypeClasses #-}
{-# language LambdaCase #-}
module Example where

import Control.Lens
import Control.Lens.Plated1
import Control.Monad
import Control.Monad.Unify

data Ty a = TyArr (Ty a) (Ty a) | TyVar a
  deriving (Eq, Show, Ord, Functor, Foldable, Traversable)

instance AsVar Ty where
  _Var = prism' TyVar (\case; TyVar a -> Just a; _ -> Nothing)

instance Plated1 Ty where
  plate1 f (TyArr a b) = TyArr <$> f a <*> f b
  plate1 _ a = pure a

instance Unifiable Ty where
  toplevelEqual TyArr{} TyArr{} = True
  toplevelEqual (TyVar a) (TyVar b) = a == b
  toplevelEqual _ _ = False

instance HasAnnotation Ty () where

instance Applicative Ty where pure = TyVar; (<*>) = ap
instance Monad Ty where
  return = TyVar
  TyVar a      >>= f = f a
  TyArr a b >>= f = TyArr (a >>= f) (b >>= f)

do_it :: Either (UnificationError (Ty (Either UVar String)) ()) (Maybe (Ty String))
do_it = runUnifyT $ do
  a <- fresh
  b <- fresh
  let t1 = TyArr (TyVar $ Right "a") (TyArr (TyVar $ Left b) $ TyVar $ Left a)
  let t2 = TyArr (TyVar $ Left a) (TyArr (TyVar $ Left a) $ TyVar $ Right "a")
  unify t1 t2
  freeze t1
