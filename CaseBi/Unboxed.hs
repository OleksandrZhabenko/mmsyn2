-- |
-- Module      :  CaseBi.Unboxed
-- Copyright   :  (c) OleksandrZhabenko 2019-2020
-- License     :  MIT
-- Stability   :  Experimental
-- Maintainer  :  olexandr543@yahoo.com
--

module CaseBi.Unboxed (
-- * Function that can be used instead of @case ... of@ construction
--
-- > case var of 
-- >   a1 -> b1
-- >   a2 -> b2
-- >   a3 -> b3
-- >   ...
-- >   an -> bn
-- >   _  -> def
-- 
-- for efficiency or other data representation
  getBFst', getBFst, getBFstV, 
-- * Additional functions that are used to sort a list of pairs (which can be obtained e. g. by 'Prelude.zip')
  sortFst, sortFstV,
-- ** Function that can be used for changing the @Vector (a, b)@ during its creation 
  filterP
) where

import qualified Data.Vector.Unboxed as V (Vector,unsafeIndex,unsafeSlice,length,fromList,map)
import qualified Data.List as L (groupBy,nubBy)
import Data.Vector.Unboxed.Base

-- | A variant of the 'CaseBi.getBFst'' function that operates on the unboxed 'V.Vector'.
-- 
getBFst' 
  :: (Ord a, Unbox a, Unbox b) 
  => (b, V.Vector (a, b)) -- ^ @b@ is a default value that can be substituted if there is no correspendence in the set of @(a, b)@ tuples (the 'otherwise' or irrefutable pattern analogue). Vector of the @(a, b)@ tuples that must be sorted in ascending order for the first argument. If there are several pairs @(a, b)@ with the same @a@, the function gives a resulting @b@ as if there is only the first one
  -> a -- ^ an element for which the corresponding resulting b must be found
  -> b -- ^ the result
getBFst' (def, vec) l | if compare l (fst (V.unsafeIndex vec 0)) == LT then True else compare l (fst (V.unsafeIndex vec (V.length vec - 1))) == GT = def
                      | compare (V.length vec) 2 /= LT = if compare l (fst (V.unsafeIndex vec ((V.length vec `quot` 2) - 1))) /= GT
  then getBFst' (def, (V.unsafeSlice 0 (V.length vec `quot` 2) vec)) l 
  else getBFst' (def, (V.unsafeSlice (V.length vec `quot` 2) (V.length vec - (V.length vec `quot` 2)) vec)) l 
                      | otherwise = snd (V.unsafeIndex vec 0)
{-# INLINABLE getBFst' #-}
                   
-- | A variant of the 'CaseBi.getBFstV' that operates on the unboxed 'V.Vector'.
-- 
getBFstV :: (Ord a, Unbox a, Unbox b) => V.Vector (a, b) -- ^ Vector of the @(a, b)@ tuples that are sorted in ascending order for the first argument
  -> b -- ^ a default value that can be substituted if there is no correspendence in the set of @(a, b)@ tuples (the 'otherwise' or irrefutable pattern analogue)
  -> V.Vector a -- ^ a Vector needed to be transformed accordingly to the correct @(a, b)@ tuple pairs
  -> V.Vector b -- ^ the resulting Vector
getBFstV c y = V.map (getBFst' (y, c))
{-# INLINE getBFstV #-}

-- | A variant of the 'CaseBi.getBFst' that operates on the unboxed 'V.Vector'.
-- 
getBFst :: (Ord a, Unbox a, Unbox b) => V.Vector (a, b) -- ^ Vector of the @(a, b)@ tuples that must be sorted in ascending order for the first argument
  -> b -- ^ a default value that can be substituted if there is no correspendence in the set of @(a, b)@ tuples (the 'otherwise' or irrefutable pattern analogue)
  -> [a] -- ^ a list of values needed to be transformed accordingly to the correct @(a, b)@ tuple pairs
  -> [b] -- ^ the resulting list
getBFst c y = map (getBFst' (y, c))
{-# INLINE getBFst #-}

-- | A variant of the 'CaseBi.sortFst' that operates on the unboxed 'V.Vector'. It is inspired by the work: https://wiki.haskell.org/Introduction
sortFst :: (Ord a, Unbox a, Unbox b) => [(a, b)] -> [(a, b)]
sortFst xs = if null xs then [] else sortFst (filter (\(x, _) -> compare x (fst (head xs)) == LT) xs) ++ filter (\(x, _) -> x == (fst (head xs))) xs ++ 
  sortFst (filter (\(x, _) -> compare x (fst (head xs)) == GT) xs)
{-# INLINABLE sortFst #-}           

-- | A variant of the 'CaseBi.sortFstV' that operates on the unboxed 'V.Vector'.
--
sortFstV 
  :: (Ord a, Unbox a, Unbox b) => [(a, b)] -- ^ The list of conditions that is then converted to the corresponding Vector
   -> V.Vector (a, b) -- ^ the resulting sorted Vector that can be used further in getBFst' and its successors.
sortFstV = V.fromList . L.nubBy (\(x, _) (y, _) -> x == y) . sortFst
{-# INLINE sortFstV #-}

-- | A variant of the 'CaseBi.filterP' that operates on the unboxed 'V.Vector'.
filterP 
  :: (Ord a, Unbox a, Unbox b) => ((a, b) -> Bool) -- ^ The predicate @p@ used to select the only one value of @b@ in the pairs @(a, b)@ with the same @a@. 
  -- ^ If there are several pairs @(a, b)@ for the same @a@ that satisfies a predicate then the first one is used. For large @[(a, b)]@ 
  -- ^ it can be rather complex.
  -> [(a, b)] -- ^ The list of @(a, b)@ sorted in the ascending order by the first element a (e. g. by the 'sortFst' function)
  -> V.Vector (a, b) -- ^ The resulting filtered @Vector (a, b)@ that can be used for getFstB' and its successor functions.
--
filterP p xs = V.fromList . concatMap (\x -> take 1 . dropWhile (not . p) $ x) . L.groupBy (\(x1,_) (x2,_) -> x1 == x2) $ xs
{-# INLINE filterP #-}
