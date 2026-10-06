-----------------------------------------------------------------------------
--
-- Module      :  $Headers
-- License     :  Apache 2.0
--
-- Stability   :  Experimental
-- Portability :  Portable
--

-----------------------------------------------------------------------------

-- | Independent implementation of association-map functions.
module Spec.Marlowe.Semantics.AssocMap (
  -- * Utility functions
  assocMapAdd,
  assocMapDelete,
  assocMapEq,
  assocMapInsert,
  assocMapLookup,
  assocMapMember,
  assocMapSort,
  assocMapValid,
) where

import Data.Function (on)
import Data.List (groupBy, nub, sortBy)

import qualified PlutusTx.AssocMap as AM (Map, toList, unsafeFromList)

-- | Fail if the map contains duplicate keys.
assocMapValid :: (Eq k) => AM.Map k v -> AM.Map k v
assocMapValid am =
  let keys = fst <$> AM.toList am
   in if nub keys == keys
        then am
        else error "Duplicate keys in PlutusTx.AssocMap.Map."

-- | Sort a map by key.
assocMapSort :: (Ord k) => AM.Map k v -> AM.Map k v
assocMapSort = AM.unsafeFromList . sortBy (compare `on` fst) . AM.toList

-- | Test equality of maps.
assocMapEq :: (Ord k) => (Eq v) => AM.Map k v -> AM.Map k v -> Bool
assocMapEq = (==) `on` assocMapSort

-- | Insert an entry into a map.
assocMapInsert :: (Eq k) => k -> v -> AM.Map k v -> AM.Map k v
assocMapInsert k v =
  AM.unsafeFromList
    . ((k, v) :)
    . filter ((/= k) . fst)
    . AM.toList

-- | Insert an entry into a map.
assocMapAdd :: (Ord k) => (Num v) => k -> v -> AM.Map k v -> AM.Map k v
assocMapAdd k v = do
  let sumStep [] = error "Impossible: empty group in assocMapAdd."
      sumStep groupped@((key, _) : _) = (key, sum (snd <$> groupped))
  AM.unsafeFromList
    . fmap sumStep
    . groupBy ((==) `on` fst)
    . sortBy (compare `on` fst)
    . ((k, v) :)
    . AM.toList

-- | Check membership of a key in a map.
assocMapMember :: (Eq k) => k -> AM.Map k v -> Bool
assocMapMember k = any ((== k) . fst) . AM.toList

-- | Lookup a value in a map.
assocMapLookup :: (Eq k) => k -> AM.Map k v -> Maybe v
assocMapLookup k = lookup k . AM.toList

-- | Delete a key from a map.
assocMapDelete :: (Eq k) => k -> AM.Map k v -> AM.Map k v
assocMapDelete k = AM.unsafeFromList . filter ((/= k) . fst) . AM.toList
