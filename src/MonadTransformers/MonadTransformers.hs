{-# LANGUAGE InstanceSigs #-}

module MonadTransformers.MonadTransformers where

newtype Identity a =
  Identity { runIdentity :: a }

newtype Compose f g a =
  Compose { getCompose :: f (g a) }
  deriving (Eq, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance (Functor f, Functor g)
         => Functor (Compose f g) where
  fmap f (Compose fga) =
    Compose $ (fmap . fmap) f fga

instance (Applicative f, Applicative g)
         => Applicative (Compose f g) where
  pure :: a -> Compose f g a
  pure a = Compose $ (pure . pure) a

  (<*>) :: Compose f g (a -> b)
        -> Compose f g a
        -> Compose f g b
  (Compose f) <*> (Compose a) = Compose $ ((<*>) $ fmap (<*>) f) a

instance (Foldable f, Foldable g)
         => Foldable (Compose f g) where
  foldMap f (Compose fga) = foldMap (foldMap f) fga

instance (Traversable f, Traversable g)
         => Traversable (Compose f g) where
  traverse f (Compose fga) = Compose <$> traverse (traverse f) fga

----------------------
-- Bifunctor
----------------------

class Bifunctor p where
  bimap :: (a -> b)
        -> (c -> d)
        -> p a c
        -> p b d
  bimap f g = first f . second g

  first :: (a -> b) -> p a c -> p b c
  first f = bimap f id

  second :: (b -> c) -> p a b -> p a c
  second = bimap id

data Deux a b = Deux a b

instance Bifunctor Deux where
  bimap f g (Deux a b) = Deux (f a) (g b)
  first f (Deux a b)   = Deux (f a) b
  second f (Deux a b)  = Deux a (f b)


data Const a b = Const a

instance Bifunctor Const where
  bimap f _ (Const a) = Const $ f a
  first f (Const a)   = Const $ f a
  second _ (Const a)  = Const a


data Drei a b c = Drei a b c

instance Bifunctor (Drei a) where
  bimap f g (Drei a b c) = Drei a (f b) (g c)
  first f (Drei a b c)   = Drei a (f b) c
  second f (Drei a b c)  = Drei a b (f c)


data SuperDrei a b c = SuperDrei a b

instance Bifunctor (SuperDrei a) where
  bimap f _ (SuperDrei a b) = SuperDrei a (f b)
  first f (SuperDrei a b)   = SuperDrei a (f b)
  second _ (SuperDrei a b)  = SuperDrei a b


data SemiDrei a b c = SemiDrei a

instance Bifunctor (SemiDrei a) where
  bimap _ _ (SemiDrei a) = SemiDrei a
  first _ (SemiDrei a)   = SemiDrei a
  second _ (SemiDrei a)  = SemiDrei a

data Either' a b =
    Left' a
  | Right' b

instance Bifunctor Either' where
  bimap f _ (Left' a)  = Left' (f a)
  bimap _ g (Right' a) = Right' (g a)

  first f (Left' a)    = Left' (f a)
  first _ (Right' a)   = Right' a

  second _ (Left' a)   = Left' a
  second f (Right' a)  = Right' (f a)

----------------------
-- IdentityT
----------------------

newtype IdentityT f a =
  IdentityT { runIdentityT :: f a }
  deriving (Eq, Show)

instance (Functor m)
         => Functor (IdentityT m) where
  fmap f (IdentityT fa) =
    IdentityT (fmap f fa)

instance (Applicative m)
         => Applicative (IdentityT m) where
  pure x = IdentityT $ pure x

  (IdentityT fab) <*> (IdentityT fa) =
    IdentityT (fab <*> fa)

instance (Monad m)
         => Monad (IdentityT m) where
  return = pure

  (>>=) :: IdentityT m a
        -> (a -> IdentityT m b)
        -> IdentityT m b
  (IdentityT ma) >>= f = IdentityT $ ma >>= runIdentityT . f

----------------------
-- MaybeT
----------------------

newtype MaybeT m a =
  MaybeT { runMaybeT :: m (Maybe a) }

instance (Functor m)
         => Functor (MaybeT m) where
  fmap f (MaybeT ma) =
    MaybeT $ (fmap . fmap) f ma

instance (Applicative m)
         => Applicative (MaybeT m) where
  pure x = MaybeT (pure (pure x))

  (MaybeT fab) <*> (MaybeT mma) =
    MaybeT $ (<*>) <$> fab <*> mma

instance (Monad m)
         => Monad (MaybeT m) where
  return = pure
  (>>=) :: MaybeT m a
        -> (a -> MaybeT m b)
        -> MaybeT m b
  (MaybeT ma) >>= f = MaybeT $ do
    a <- ma
    case a of
      Nothing -> pure Nothing
      Just x -> runMaybeT $ f x

----------------------
-- EitherT
----------------------

newtype EitherT e m a =
  EitherT { runEitherT :: m (Either e a) }

instance Functor m
         => Functor (EitherT e m) where
  fmap f (EitherT ema) = EitherT $ (fmap . fmap) f ema

instance Applicative m
         => Applicative (EitherT e m) where
  pure x = EitherT $ (pure . pure) x
  (EitherT f) <*> (EitherT g) = EitherT $ (<*>) <$> f <*> g

instance Monad m
         => Monad (EitherT e m) where
  return = pure
  (EitherT ema) >>= f = EitherT $ do
    a <- ema
    case a of
      Left x -> return $ Left x
      Right x -> runEitherT $ f x

swapEither :: Either a b -> Either b a
swapEither mea =
  case mea of
    Left x  -> Right x
    Right x -> Left x

swapEitherT :: Functor m
            => EitherT e m a
            -> EitherT a m e
swapEitherT (EitherT mea) = EitherT $ swapEither <$> mea

eitherT :: Monad m =>
           (a -> m c)
        -> (b -> m c)
        -> EitherT a m b
        -> m c
eitherT f g (EitherT amb) = either f g =<< amb

----------------------
-- ReaderT
----------------------

newtype ReaderT r m a =
  ReaderT { runReaderT :: r -> m a }

instance (Functor m)
         => Functor (ReaderT r m) where
  fmap f (ReaderT rma) = ReaderT (fmap f . rma)
  -- ReaderT $ (fmap . fmap) f rma

instance (Applicative m)
         => Applicative (ReaderT r m) where
  pure a = ReaderT $ (pure . pure) a
  (ReaderT fmab) <*> (ReaderT rma) =
    ReaderT $ (<*>) <$> fmab <*> rma

instance (Monad m)
         => Monad (ReaderT r m) where
  return = pure
  (>>=) :: ReaderT r m a
        -> (a -> ReaderT r m b)
        -> ReaderT r m b
  (ReaderT rma) >>= f =
    ReaderT $ \r -> do
      a <- rma r
      let p  = f a
      runReaderT p r

----------------------
-- StateT
----------------------

newtype StateT s m a =
  StateT { runState :: s -> m (a,s) }

instance Functor m
      => Functor (StateT s m) where
      fmap f (StateT sma) =
        StateT $ (fmap . fmap) tup sma
        where
          tup (a, b) = (f a, b)

instance (Monad m)
      => Applicative (StateT s m) where
  pure a = StateT $ \s -> pure (a, s)

  (<*>) :: StateT s m (a -> b) -> StateT s m a -> StateT s m b
  (StateT f) <*> (StateT sma) = StateT $ \s -> do
    (g, s') <- f s
    (x,s'') <- sma s'
    return (g x , s'')

instance Monad m
      => Monad (StateT s m) where
  return = pure
  (StateT sma) >>= f = StateT $ \s -> do
    (x, s') <- sma s
    runState (f x) s'
