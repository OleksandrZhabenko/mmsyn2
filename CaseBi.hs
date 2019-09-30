--{-# LANGUAGE BangPatterns #-}

module CaseBi (
-- * Function that can be used instead of 'case ... of' construction
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
-- * Additional functions that are used to sort a list of pairs (which can be obtained e. g. by Prelude.zip)
  sortFst, sortFstV,
-- ** Function that can be used for changing the Vector (a, b) during its creation 
  filterP
) where

import qualified Data.Vector as V (Vector,unsafeIndex,unsafeSlice,length,fromList,map)
import qualified Data.List as L (groupBy,nubBy)
--import Prelude
-- (Bool,Eq,Ord,map,(>=),(<=),(>),(<),(==),(&&),(.),(++),(-),($),filter,otherwise,fst,snd,quot,not,null,dropWhile,concatMap,take,seq,undefined)

-- | The function that can be used instead of the 'case ... of' function
--
-- > case var of
-- >   a1 -> b1
-- >   a2 -> b2
-- >   a3 -> b3
-- >   ...
-- >   an -> bn
-- >   _  -> defaultValue
-- 
-- If we follow a lot of teaching materials that explain the workflow of the construction we think that the complexity of it is about /O(n)/ for the transformation of @a@ to @b@ here. 
-- David Feuer (david.feuer (at) gmail.com) said that 'case ... of' is already optimized in GHC. Some benchmarks show that its behaviour  tends to be about of /O(log n)/ complexity, the same as
-- the proposed function getBFst'. Nevertheless, the last one shows better performance in some situations, is rather general and can be used for another data representation.
-- Therefore, it can be preferred in some situations. getBFst' uses binary search algorithm and a Vector (a, b) as somewhat like a complicated filter or like a special sieve.
-- The @Vector (a, b)@ must be sorted in ascending order here for the algorithm to be used correctly. For this you can use 
-- the following functions 'sortFst' and 'sortFstV'. 
-- 
-- @b@ before @Vector (a, b)@ in the tuple in the type definition of the 'getBFst' must be a @defaultValue@ for 'case' above. 
--
-- @Vector (a, b)@ corresponds to 
--
-- >  a1 -> b1
-- >  a2 -> b2
-- >  a3 -> b3
-- >  ...
-- >  an -> bn
-- 
getBFst' 
  :: (Ord a) => (b, V.Vector (a, b)) -- ^ @b@ is a default value that can be substituted if there is no correspendence in the set of @(a, b)@ tuples (the 'otherwise' or irrefutable pattern analogue).
  -- ^ Vector of the @(a, b)@ tuples that must be sorted in ascending order for the first argument. If there are several pairs @(a, b)@ with the same @a@, 
  -- ^ the function gives a resulting @b@ as if there is only the first one
  -> a -- ^ an element for which the corresponding resulting b must be found
  -> b -- ^ the result
getBFst' (def, vec) l | if compare l (fst (V.unsafeIndex vec 0)) == LT then True else compare l (fst (V.unsafeIndex vec (V.length vec - 1))) == GT = def
                      | compare (V.length vec) 2 /= LT = if compare l (fst (V.unsafeIndex vec ((V.length vec `quot` 2) - 1))) /= GT
  then getBFst' (def, (V.unsafeSlice 0 (V.length vec `quot` 2) vec)) l 
  else getBFst' (def, (V.unsafeSlice (V.length vec `quot` 2) (V.length vec - (V.length vec `quot` 2)) vec)) l 
                      | otherwise = snd (V.unsafeIndex vec 0)
{-# INLINE getBFst' #-}
                   
-- | The function that uses special realization of the binary search to effectively transform the @Vector a@ to @Vector b@ instead of simply use 
--
-- > case var of
-- >   a1 -> b1
-- >   a2 -> b2
-- >   a3 -> b3
-- >   ...
-- >   an -> bn
-- >   _  -> defaultValue
-- 
-- The @Vector (a, b)@ must be sorted in ascending order here for the algorithm to be used correctly. For this you can use 
-- the following functions 'sortFst' and 'sortFstV'. it can be used to simplify the procedure for optimizing the code for transformation of the Vector data.
-- 
-- @b@ after @Vector (a, b)@ in the type definition of the 'getBFstV' must be a @defaultValue@ for 'case' above. 
--
-- @Vector (a, b)@ corresponds to 
--
-- >  a1 -> b1
-- >  a2 -> b2
-- >  a3 -> b3
-- >  ...
-- >  an -> bn
-- 
getBFstV :: (Ord a) => V.Vector (a, b) -- ^ Vector of the @(a, b)@ tuples that are sorted in ascending order for the first argument
  -> b -- ^ a default value that can be substituted if there is no correspendence in the set of @(a, b)@ tuples (the 'otherwise' or irrefutable pattern analogue)
  -> V.Vector a -- ^ a Vector needed to be transformed accordingly to the correct @(a, b)@ tuple pairs
  -> V.Vector b -- ^ the resulting Vector
getBFstV c y = V.map (getBFst' (y, c))
{-# INLINE getBFstV #-}

-- | The function that uses special kind of bisection to effectively transform the @[a]@ to @[b]@ instead of simply use 
--
-- > case var of
-- >   a1 -> b1
-- >   a2 -> b2
-- >   a3 -> b3
-- >   ...
-- >   an -> bn
-- >   _  -> defaultValue
-- 
-- The @Vector (a, b)@ must be sorted in ascending order here for the algorithm to be used correctly. For this you can use 
-- the following functions 'sortFst' and 'sortFstV'. The function can be used to simplify the procedure for optimizing the code for transformation of the list data 
-- or to represent the data in another way.
-- 
-- @b@ after @Vector (a, b)@ in the type definition of the 'getBFst' must be a @defaultValue@ for 'case' above. 
--
-- @Vector (a, b)@ corresponds to 
--
-- >  a1 -> b1
-- >  a2 -> b2
-- >  a3 -> b3
-- >  ...
-- >  an -> bn
-- 
getBFst :: (Ord a) => V.Vector (a, b) -- ^ Vector of the @(a, b)@ tuples that must be sorted in ascending order for the first argument
  -> b -- ^ a default value that can be substituted if there is no correspendence in the set of @(a, b)@ tuples (the 'otherwise' or irrefutable pattern analogue)
  -> [a] -- ^ a list of values needed to be transformed accordingly to the correct @(a, b)@ tuple pairs
  -> [b] -- ^ the resulting list
getBFst c y = map (getBFst' (y, c))
{-# INLINE getBFst #-}

-- | Function that sorts a list of @(a, b)@ tuples by the first argument 
-- and is inspired by Data.List.sort function (the last one can be used for sorting the @(a, b)@ tuples where both the types of @a@ and @b@
-- have instances of the class Ord). It is inspired by the work: https://wiki.haskell.org/Introduction
sortFst :: (Ord a) => [(a, b)] -> [(a, b)]
sortFst xs = if null xs then [] else sortFst (filter (\(x, _) -> compare x (fst (head xs)) == LT) xs) ++ filter (\(x, _) -> x == (fst (head xs))) xs ++ 
  sortFst (filter (\(x, _) -> compare x (fst (head xs)) == GT) xs)
{-# INLINE sortFst #-}           

-- | Function that prepares the list of @(a, b)@ tuples representing the 
--
-- > case var of 
-- >   a1 -> b1
-- >   a2 -> b2
-- >   a3 -> b3
-- >    ...
-- >   an -> bn
-- >   _  -> defaultValue
--
-- for usage in the 'getBFst' and 'getBFstV' functions. 
--
-- The resulting vector has for every @a@ only one element, which was the first in the list of tuples @(a, b)@ after sorting by 'sortFst' function.
--
sortFstV 
  :: (Ord a) => [(a, b)] -- ^ The list of conditions that is then converted to the corresponding Vector
   -> V.Vector (a, b) -- ^ the resulting sorted Vector that can be used further in getBFst' and its successors.
sortFstV = V.fromList . L.nubBy (\(x, _) (y, _) -> x == y) . sortFst
{-# INLINE sortFstV #-}

-- | The function that is used to filter @[(a, b)]@ of the corresponding values for getFstB' to obtain the @Vector (a, b)@ 
-- such that the @b@ element for the sequence of pairs @(a, b)@ with the same @a@ is selected by the predicate @p@ and is not necessarily the first one 
-- as it is for the getFstB' function and its successors by default.
filterP 
  :: (Ord a) => ((a, b) -> Bool) -- ^ The predicate @p@ used to select the only one value of @b@ in the pairs @(a, b)@ with the same @a@. 
  -- ^ If there are several pairs @(a, b)@ for the same @a@ that satisfies a predicate then the first one is used. For large @[(a, b)]@ 
  -- ^ it can be rather complex.
  -> [(a, b)] -- ^ The list of @(a, b)@ sorted in the ascending order by the first element a (e. g. by the 'sortFst' function)
  -> V.Vector (a, b) -- ^ The resulting filtered @Vector (a, b)@ that can be used for getFstB' and its successor functions.
--
-- Example: 
--
-- > filterP (\(t, w) -> (t == "1") || (w > 'f')) . sortFst $ [("1",'a'),("4a",'k'),("4a",'b'),("4a",'c'),("4a",'d'),("4a",'e'),("b7",'c'),("b7",'k')] = [("1",'a'),("4a",'k'),("b7",'k')]
-- 
filterP p xs = V.fromList . concatMap (\x -> take 1 . dropWhile (not . p) $ x) . L.groupBy (\(x1,_) (x2,_) -> x1 == x2) $ xs
{-# INLINE filterP #-}
