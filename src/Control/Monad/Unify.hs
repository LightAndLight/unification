{-# language DefaultSignatures #-}
{-# language DeriveFunctor #-}
{-# language DeriveFoldable #-}
{-# language DeriveTraversable #-}
{-# language FlexibleInstances #-}
{-# language FunctionalDependencies #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language LambdaCase #-}
{-# language MultiParamTypeClasses #-}
{-# language StandaloneDeriving #-}
{-# language RankNTypes #-}
{-# language TemplateHaskell #-}
{-# language TypeFamilies #-}
{-# language UndecidableInstances #-}
module Control.Monad.Unify
  ( module Control.Lens.Plated1
    -- * Monad
  , UnifyT
  , runUnifyT
  , UVar
  , UTerm
  , uterm
    -- * Operations
  , fresh
  , occurs
  , union
  , find
  , unify
  , unfreeze
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
import Data.Deriving
import Data.Function
import Data.Functor.Classes
import Data.Functor.Compose
import Data.Equivalence.Monad
import Data.Foldable hiding (find)

-- | Unification variables. Use 'fresh' to obtain new 'UVar's
newtype UVar = UVar Int deriving (Eq, Show, Ord)

-- | Unification terms. Convert terms into unification terms using 'unfreeze',
-- and convert unification terms back into regular terms using 'freeze'
newtype UTerm t v = UTerm { getUTerm :: Compose t (Either UVar) v }
  deriving (Eq, Show, Ord, Functor, Foldable, Traversable, Applicative)
deriveEq1 ''UTerm
deriveShow1 ''UTerm
deriveOrd1 ''UTerm
makeWrapped ''UTerm

instance Monad t => Monad (UTerm t) where
  UTerm (Compose u) >>= f = UTerm . Compose $ do
    u' <- u
    case u' of
      Left e -> pure $ Left e
      Right a -> getCompose . getUTerm $ f a

-- | 'Iso'' on 'UTerm's.
--
-- @'view' ('from' 'uterm') :: 'UTerm' t v -> t ('Either' 'UVar' v)@
--
-- @'view' 'uterm' :: t ('Either' 'UVar' v) -> 'UTerm' t v@
uterm :: Iso' (t (Either UVar v)) (UTerm t v)
uterm = iso (UTerm . Compose) (getCompose . getUTerm)

-- | Concrete unification error datatype
data UnificationError term var ann
  = OccursError UVar (UTerm term var) ann
  | MismatchError (UTerm term var) (UTerm term var) ann
  deriving (Eq, Show)

instance AsUnificationError (UnificationError term var ann) term var ann where
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

newtype E a b c d = E { getE :: forall s. EquivT s a b c d }

instance Functor c => Functor (E a b c) where
  fmap f (E a) = E $ fmap f a

instance (Applicative c, Monad c) => Applicative (E a b c) where
  pure a = E $ pure a
  E f <*> E a = E $ f <*> a

instance Monad c => Monad (E a b c) where
  E a >>= f = E $ a >>= (getE . f)

newtype UnifyT t v m a = UnifyT { runUnifyT' :: StateT Int (E (UTerm t v) (UTerm t v) m) a }

instance Functor m => Functor (UnifyT t v m) where
  fmap f (UnifyT a) = UnifyT $ fmap f a

instance (Applicative m, Monad m) => Applicative (UnifyT t v m) where
  pure a = UnifyT $ pure a
  UnifyT f <*> UnifyT a = UnifyT $ f <*> a

instance Monad m => Monad (UnifyT t v m) where
  UnifyT a >>= f = UnifyT $ a >>= (runUnifyT' . f)

instance MonadState s m => MonadState s (UnifyT t v m) where
  get = UnifyT . lift $ E get
  put a = UnifyT . lift $ E (put a)

instance MonadError e m => MonadError e (UnifyT t v m) where
  catchError a b =
    UnifyT $ StateT $ \s -> E ((,) <$> catchError (getE . flip evalStateT s $ runUnifyT' a) (getE . flip evalStateT s . runUnifyT' . b) <*> pure s)
  throwError a = UnifyT . lift $ E (throwError a)

instance MonadWriter w m => MonadWriter w (UnifyT t v m) where
  writer a = UnifyT . lift $ E (writer a)
  tell a = UnifyT . lift $ E (tell a)
  listen a =
    UnifyT $ StateT $ \s -> E ((,) <$> listen (getE . flip evalStateT s $ runUnifyT' a) <*> pure s)
  pass a =
    UnifyT $ StateT $ \s -> E ((,) <$> pass (getE . flip evalStateT s $ runUnifyT' a) <*> pure s)

instance MonadReader r m => MonadReader r (UnifyT t v m) where
  ask = UnifyT . lift $ E ask
  local a b =
    UnifyT $ StateT $ \s -> E ((,) <$> local a (getE . flip evalStateT s $ runUnifyT' b) <*> pure s)

instance MonadTrans (UnifyT t v) where
  lift m = UnifyT $ lift (E $ lift m)

-- | Generate a fresh 'UVar'
fresh :: Monad m => UnifyT t v m UVar
fresh = UnifyT $ do
  count <- get
  put $! count+1
  pure $ UVar count

-- | Unify two terms
unify
  :: ( AsVar t
     , HasAnnotation t ann
      , Unifiable t
     , Plated1 t
     , Ord1 t
     , Ord v
     , AsUnificationError e t v ann
     , MonadError e m
     )
  => UTerm t v
  -> UTerm t v
  -> UnifyT t v m ()
unify m n = do
  a <- view (from uterm) <$> find m
  b <- view (from uterm) <$> find n
  if toplevelEqual a b
    then traverse_ (uncurry . on unify $ view uterm) (zip (a ^.. plate1) (b ^.. plate1))
    else
      case (a ^? _Var, b ^? _Var) of
        (Just (Left a'), _)
          | occurs a' (b ^. uterm) ->
            throwError $ _OccursError # (a', b ^. uterm, a ^. annotation)
          | otherwise -> (union `on` view uterm) a b
        (_, Just (Left b'))
          | occurs b' (a ^. uterm) ->
            throwError $ _OccursError # (b', a ^. uterm, a ^. annotation)
          | otherwise -> (union `on` view uterm) a b
        (_, _) ->
          unless (on (==) (view uterm) a b) . throwError $
          _MismatchError # (a ^. uterm, b ^. uterm, a ^. annotation)

-- | Union the equivalence classes of two terms
union :: (Monad m, Ord1 t, Ord v) => UTerm t v -> UTerm t v -> UnifyT t v m ()
union a b = UnifyT . lift $ E (equate a b)

-- | Find the representative of term's equivalence class
find
  :: (Monad m, Plated1 t, Ord1 t, Ord v)
  => UTerm t v -> UnifyT t v m (UTerm t v)
find a = do
  repr <- UnifyT $ lift $ E (classDesc a)
  (^. uterm) <$>
    -- HELP ME
    traverseOf
      plate1
      (fmap (^. from uterm) . find . (^. uterm))
      (repr ^. from uterm)

-- | Check whether or not a 'UVar' is present in a term
occurs :: (AsVar t, Plated1 t) => UVar -> UTerm t a -> Bool
occurs n t = n `elem` (t ^.. from uterm.cosmos1._Var._Left)

runUnifyT :: (AsVar t, Monad m, Plated1 t) => UnifyT t v m res -> m res
runUnifyT a = runEquivT id combine (getE . flip evalStateT 0 $ runUnifyT' a)
  where
    combine u v =
      case u ^? from uterm._Var._Left of
        Just{} -> v
        Nothing -> case v ^? from uterm._Var._Left of
          Just{} -> u
          Nothing
            | lengthOf (from uterm.plate1._Var._Left) u <
              lengthOf (from uterm.plate1._Var._Left) v -> u

-- | Convert a term into a unifiable term
unfreeze :: Functor t => t v -> UTerm t v
unfreeze = UTerm . Compose . fmap Right

-- | Attempt to convert a unifiable term into a regular term. Produces a 'Nothing' if
-- the term still contains unsolved unification variables
freeze
  :: ( AsVar t 
     , Monad m
     , Ord1 t
     , Ord v
     , Plated1 t
     , Traversable t
     , Monad t
     )
  => UTerm t v
  -> UnifyT t v m (Maybe (t v))
freeze = fmap (fmap join . sequence) . go . view (from uterm)
  where
    go =
      traverse $ \x -> case x of
        Right a -> pure . Just $ _Var # a
        _ -> do
          r <- find (view uterm $ _Var # x)
          case r ^.. from uterm.cosmos1._Var._Left of
            [] -> freeze r
            _ -> pure Nothing

-- | Terms that can be unified
class Unifiable term where
  -- |
  -- Checks for top-level structural equality, for example:
  --
  -- @
  -- data Ty = TyArr Ty Ty | TyVar String
  -- instance Unifiable Ty where
  --   toplevelEqual TyArr{} TyArr{} = True
  --   toplevelEqual _ _ = False
  -- @
  --
  -- Must obey the law:
  -- @forall t u. toplevelEqual t u ==> 'lengthOf' 'plate1' t == 'lengthOf' 'plate1' u@
  -- i.e. top-level equal terms must have the same number of children
  --
  -- Should obey the law:
  -- @forall t u. 'hasn't' 'plate1' t || 'hasn't' 'plate1' u ==> 'not' (toplevelEqual t u)@
  -- i.e. a term with no children should not be top-level equal to another
  -- term
  toplevelEqual :: term a -> term a -> Bool

-- | Terms from which 'variables' can be extracted
class AsVar term where
  _Var :: Prism' (term var) var

-- | Datatypes which can contain unification errors.
class AsUnificationError e term var ann | e -> term var ann where
  _OccursError :: Prism' e (UVar, UTerm term var, ann)
  _MismatchError :: Prism' e (UTerm term var, UTerm term var, ann)

-- | Terms from which 'annotations' can be extracted
--
-- For terms that do not contain annotations, use the default instance:
--
-- @instance HasAnnotation Term ()@
class HasAnnotation term ann | term -> ann where
  annotation :: Lens' (term a) ann

  default annotation :: (ann ~ ()) => Lens' (term a) ann
  annotation = lens (const ()) const
