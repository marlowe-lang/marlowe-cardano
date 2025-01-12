{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Concurrent.Component where

import Colog (Message, WithLog, logError)
import Control.Applicative (liftA2)
import Control.Arrow (
  Arrow (arr, first, second, (&&&), (***)),
  ArrowApply (app),
  ArrowChoice (left, right, (+++), (|||)),
  ArrowLoop (loop),
 )
import Control.Category (Category (id, (.)))
import Control.Monad (forever)
import Data.Functor (void)
import Data.String (fromString)
import UnliftIO (
  Concurrently (..),
  Exception (displayException),
  MonadUnliftIO,
  STM,
  SomeException (SomeException),
  atomically,
  catch,
  throwIO,
  try,
 )
import UnliftIO.Concurrent (forkIO)
import Prelude hiding ((.))

newtype Component m a b = Component {unComponent :: a -> STM (Concurrently m (), b)}
  deriving (Functor)

instance (MonadUnliftIO m) => Applicative (Component m a) where
  pure = Component . pure . pure . pure
  cf <*> cx = Component $ (liftA2 . liftA2) (<*>) (unComponent cf) (unComponent cx)

instance (MonadUnliftIO m) => Monad (Component m a) where
  c >>= k = Component \a -> do
    (run1, x) <- unComponent c a
    (run2, b) <- unComponent (k x) a
    pure (run1 <> run2, b)

instance (MonadUnliftIO m) => Category (Component m) where
  id = Component \a -> pure (pure (), a)
  cbc . cab = Component \a -> do
    (run1, b) <- unComponent cab a
    (run2, c) <- unComponent cbc b
    pure (run1 <> run2, c)

instance (MonadUnliftIO m) => Arrow (Component m) where
  arr f = Component \a -> pure (pure (), f a)
  c1 *** c2 = Component \(a, b) -> do
    (run1, c) <- unComponent c1 a
    (run2, d) <- unComponent c2 b
    pure (run1 <> run2, (c, d))
  c1 &&& c2 = Component \a -> do
    (run1, b) <- unComponent c1 a
    (run2, c) <- unComponent c2 a
    pure (run1 <> run2, (b, c))
  first c = Component \(a, d) -> fmap (,d) <$> unComponent c a
  second c = Component \(d, a) -> fmap (d,) <$> unComponent c a

instance (MonadUnliftIO m) => ArrowChoice (Component m) where
  c1 +++ c2 =
    Component $
      either
        ((fmap . fmap) Left . unComponent c1)
        ((fmap . fmap) Right . unComponent c2)
  c1 ||| c2 = Component $ either (unComponent c1) (unComponent c2)
  left c =
    Component $
      either
        ((fmap . fmap) Left . unComponent c)
        (pure . pure . Right)
  right c =
    Component $
      either
        (pure . pure . Left)
        ((fmap . fmap) Right . unComponent c)

instance (MonadUnliftIO m) => ArrowApply (Component m) where
  app = Component $ uncurry unComponent

instance (MonadUnliftIO m) => ArrowLoop (Component m) where
  loop c = Component \a -> mdo
    (run, (b, d)) <- unComponent c (a, d)
    pure (run, b)

suppressErrors :: (MonadUnliftIO m) => Component m a b -> Component m a b
suppressErrors c = Component \a -> do
  (Concurrently run, b) <- unComponent c a
  pure (Concurrently $ void $ try @_ @SomeException run, b)

runComponent :: Component m a b -> a -> STM (m (), b)
runComponent c a = do
  (run, b) <- unComponent c a
  pure (runConcurrently run, b)

runComponent_ :: (MonadUnliftIO m) => Component m a () -> a -> m ()
runComponent_ c a = fst =<< atomically (runComponent c a)

component :: (WithLog env Message m, MonadUnliftIO m) => String -> (a -> STM (m (), b)) -> Component m a b
component name run = Component $ \a -> do
  (action, b) <- run a
  let action' =
        action `catch` \(SomeException e) -> do
          logError $ fromString $ "Component " <> name <> " crashed with exception:"
          logError $ fromString $ displayException e
          throwIO e
  pure (Concurrently action', b)

component_ :: (WithLog env Message m, MonadUnliftIO m) => String -> (a -> m ()) -> Component m a ()
component_ name = component name . fmap (pure . (,()))

serverComponent
  :: (WithLog env Message m, MonadUnliftIO m) => String -> Component m b () -> (a -> m b) -> Component m a ()
serverComponent name worker = serverComponentWithSetup name worker . (pure .)

serverComponentWithSetup
  :: forall m env a b
   . (WithLog env Message m, MonadUnliftIO m)
  => String
  -> Component m b ()
  -> (a -> STM (m b))
  -> Component m a ()
serverComponentWithSetup name worker mkAccept = component name \a -> do
  accept <- mkAccept a
  let run = forever do
        b <- accept
        void $ forkIO $ runComponent_ (suppressErrors worker) b
  pure (run, ())

hoistComponent :: (forall x. m x -> n x) -> Component m a b -> Component n a b
hoistComponent f = Component . (fmap . fmap . first) (Concurrently . f . runConcurrently) . unComponent
