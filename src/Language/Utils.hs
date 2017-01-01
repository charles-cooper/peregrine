module Language.Utils (
  InsertionMap,
  InsertionSet,
  insert,
  push,
  fromInsertionSet,
  fromInsertionMap,

  IDGenerator,
  newId,
) where

import qualified Data.Map as Map
import           Data.Map (Map(..))
import           Data.Function
import           Data.List (sortBy)
import           Data.Maybe

-- Map which keeps the insertion order of its elements
newtype InsertionMap k v = InsertionMap { getMap :: Map k (Int, v) }
type InsertionSet a = InsertionMap a ()

instance Ord k => Monoid (InsertionMap k v) where
  mempty       = InsertionMap mempty
  mappend m m' = foldr (uncurry insert) m (fromInsertionMap m')

-- Insert with NO overwriting of existing values
insert :: Ord k => k -> v -> InsertionMap k v -> InsertionMap k v
insert k v (InsertionMap m) = InsertionMap $ Map.insertWith keep k (sz, v) m
  where
    keep = flip const
    sz    = Map.size m

lookup :: Ord k => k -> InsertionMap k v -> Maybe v
lookup k = fmap snd . Map.lookup k . getMap

push :: Ord a => a -> InsertionSet a -> InsertionSet a
push a = insert a ()

fromInsertionSet :: Ord a => InsertionSet a -> [a]
fromInsertionSet = map fst . fromInsertionMap

fromInsertionMap :: Ord k => InsertionMap k v -> [(k, v)]
fromInsertionMap = Map.elems . Map.fromList . map rearrange . Map.toList . getMap
  where
    rearrange (k, (rank, v)) = (rank, (k, v))

newtype IDGenerator = IDGenerator (Map String Int)

instance Monoid IDGenerator where
  mempty  = IDGenerator mempty
  mappend = error "Not yet implemented: mappend for IDGenerator"

-- TODO come up with better name
newId :: String -> IDGenerator -> (String, IDGenerator)
newId s (IDGenerator st) = (s ++ maybe "" show midx, IDGenerator st')
  where
    (midx, st') = count s st
    count a old = let
      f _k new old = new + old
      (mret, new) = Map.insertLookupWithKey f a 1 old
      in (mret, new)

