{-# language DeriveFunctor #-}
{-# language DeriveFoldable #-}
{-# language DeriveTraversable #-}
{-# language MultiParamTypeClasses #-}
{-# language LambdaCase #-}
{-# language TemplateHaskell #-}
module Example where

import Control.Lens
import Control.Lens.Plated1
import Control.Monad
import Control.Monad.Unify
import Data.Deriving

data Ty a = TyArr (Ty a) (Ty a) | TyVar a
  deriving (Eq, Show, Ord, Functor, Foldable, Traversable)
deriveEq1 ''Ty
deriveOrd1 ''Ty
deriveShow1 ''Ty

instance AsVar Ty where
  _Var = prism' TyVar (\case; TyVar a -> Just a; _ -> Nothing)

instance Plated1 Ty where
  plate1 f (TyArr a b) = TyArr <$> f a <*> f b
  plate1 _ a = pure a

instance Unifiable Ty where
  toplevelEqual TyArr{} TyArr{} = True
  toplevelEqual _ _ = False

instance HasAnnotation Ty () where

instance Applicative Ty where pure = TyVar; (<*>) = ap
instance Monad Ty where
  return = TyVar
  TyVar a      >>= f = f a
  TyArr a b >>= f = TyArr (a >>= f) (b >>= f)

do_it :: Either (UnificationError Ty String ()) (Maybe (Ty String))
do_it = runUnifyT $ do
  a <- freshVar
  b <- freshVar
  let
    t1, t2 :: UTerm Ty String
    t1 =
      view uterm $
      TyArr (TyVar $ Right "a") (TyArr (TyVar $ Left b) $ TyVar $ Left a)
    t2 =
      view uterm $
      TyArr (TyVar $ Left a) (TyArr (TyVar $ Left a) $ TyVar $ Left b)
  unify t1 t2
  freeze t1
