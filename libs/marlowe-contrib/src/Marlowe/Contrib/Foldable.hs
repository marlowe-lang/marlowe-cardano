module Marlowe.Contrib.Foldable where

import Data.Foldable (foldlM, foldrM)
import Data.Foldable.WithIndex (FoldableWithIndex, ifoldlM)

tillFirstMatch :: (Foldable f) => f a -> (a -> Maybe b) -> Maybe b
tillFirstMatch l match = do
  let step a acc = case match a of
        res@(Just _) -> res
        _ -> acc
  foldr step Nothing l

-- Useful aliases for infix usage.

foldMapFlipped :: (Monoid m) => (Foldable f) => f a -> (a -> m) -> m
foldMapFlipped = flip foldMap

anyFlipped :: [a] -> (a -> Bool) -> Bool
anyFlipped = flip any

foldMapM
  :: (Monad m, Monoid w, Foldable t)
  => (a -> m w)
  -> t a
  -> m w
foldMapM f =
  foldlM
    ( \acc a -> do
        w <- f a
        pure $! acc <> w
    )
    mempty

foldMapMFlipped :: (Monoid b) => (Monad m) => (Foldable f) => f a -> (a -> m b) -> m b
foldMapMFlipped = flip foldMapM

ifoldMapM
  :: (Monad m, Monoid w, FoldableWithIndex i t)
  => (i -> a -> m w)
  -> t a
  -> m w
ifoldMapM f =
  ifoldlM
    ( \i acc a -> do
        w <- f i a
        pure $! acc <> w
    )
    mempty

ifoldMapMFlipped :: (Monad m, Monoid w, FoldableWithIndex i t) => t a -> (i -> a -> m w) -> m w
ifoldMapMFlipped = flip ifoldMapM

foldlMFlipped :: (Foldable t, Monad m) => b -> t a -> (b -> a -> m b) -> m b
foldlMFlipped zero foldable step = foldlM step zero foldable

foldrMFlipped :: (Foldable t, Monad m) => b -> t a -> (a -> b -> m b) -> m b
foldrMFlipped zero foldable step = foldrM step zero foldable