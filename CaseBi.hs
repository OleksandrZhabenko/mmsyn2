--{-# LANGUAGE BangPatterns #-}

module CaseBi (
-- * Function that can be used instead of 
--
-- > case var of 
-- >   a1 -> b1
-- >   a2 -> b2
-- >   a3 -> b3
-- >   ...
-- >   an -> bn
-- >   _  -> def
-- 
-- for efficiency
  getBFst', getBFst, getBFstV, 
-- * Additional functions that are used to sort a list of pairs (which can be obtained by e. g. Prelude.zip)
  sortFst, sortFstV,
-- ** Function that can be used for changing the Vector (a, b) during its creation 
  filterP
) where

import qualified Data.Vector as V (Vector,unsafeHead,unsafeLast,unsafeSlice,length,(!),fromList,map)
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
-- The function 'case a of ...' gives the /O(n)/ coplexity of the transformation of a to b here, 
-- but the function getBFst' tries to give about /O(log n)/ complexity 
-- The Vector (a, b) must be sorted in ascending order here for the algorithm to be used correctly. For this you can use 
-- the following functions 'sortFst' and 'sortFstV'. 
-- 
-- b before Vector (a, b) in the type definition of the 'getBFst' must be a @defaultValue@ for 'case' above. 
--
-- Vector (a, b) corresponds to 
--
-- >  a1 -> b1
-- >  a2 -> b2
-- >  a3 -> b3
-- >  ...
-- >  an -> bn
-- 
getBFst' :: (Ord a) => b -- ^ a default value that can be substituted if there is no correspendence in the set of (a, b) tuples (the 'otherwise' or irrefutable pattern analogue)
  -> V.Vector (a, b) -- ^ Vector of the (a, b) tuples that must be sorted in ascending order for the first argument. If there are several pairs (a, b) with the same a, 
  -- ^ the function gives a resulting b as if there is only the first one
  -> a -- ^ an element for which the corresponding resulting b must be found
  -> b -- ^ the result
getBFst' def vec l | if (l < (fst (V.unsafeHead vec))) then True else (l > (fst (V.unsafeLast vec))) = def
                   | (V.length vec >= 2) = if l <= fst (vec V.! (V.length vec `quot` 2))
  then getBFst' def (V.unsafeSlice 0 (V.length vec `quot` 2) vec) l 
  else getBFst' def (V.unsafeSlice (V.length vec `quot` 2) (V.length vec - (V.length vec `quot` 2)) vec) l 
                   | otherwise = snd (V.unsafeHead vec)
                   
-- | The function that uses special kind of bisection to effectively transform the Vector a to Vector b with  instead of simply use 
--
-- > case var of
-- >   a1 -> b1
-- >   a2 -> b2
-- >   a3 -> b3
-- >   ...
-- >   an -> bn
-- >   _  -> defaultValue
-- 
-- The function 'V.map (f (case var of ...)) [a]' gives the /O(n*m)/ coplexity of the transformation of Vector a to Vector b here 
-- where m is the length of the Vector a (and Vector b respectively here), but the function 'getBFstV' tries to give about /O(m*log n)/ complexity 
-- The Vector (a, b) must be sorted in ascending order here for the algorithm to be used correctly. For this you can use 
-- the following functions 'sortFst' and 'sortFstV'. If m >> n than the function gives more efficiency. Even otherwise, 
-- it can be used to simplify the procedure for optimizing the code for transformation of the Vector data.
-- 
-- b after Vector (a, b) in the type definition of the 'getBFstV' must be a defaultValue for case above. 
--
-- Vector (a, b) corresponds to 
--
-- >  a1 -> b1
-- >  a2 -> b2
-- >  a3 -> b3
-- >  ...
-- >  an -> bn
-- 
getBFstV :: (Ord a) => V.Vector (a, b) -- ^ Vector of the (a, b) tuples that are sorted in ascending order for the first argument
  -> b -- ^ a default value that can be substituted if there is no correspendence in the set of (a, b) tuples (the 'otherwise' or irrefutable pattern analogue)
  -> V.Vector a -- ^ a Vector needed to be transformed accordingly to the correct (a, b) tuple pairs
  -> V.Vector b -- ^ the resulting Vector
getBFstV c y = V.map (getBFst' y c)

-- | The function that uses special kind of bisection to effectively transform the [a] to [b] with  instead of simply use 
--
-- > case var of
-- >   a1 -> b1
-- >   a2 -> b2
-- >   a3 -> b3
-- >   ...
-- >   an -> bn
-- >   _  -> defaultValue
-- 
-- The function 'map (f (case var of ...)) [a]' gives the /O(n*m)/ coplexity of the transformation of [a] to [b] here 
-- where m is the length of the [a] (and [b] respectively here), but the function 'getBFst' tries to give about /O(m*log n)/ complexity 
-- The Vector (a, b) must be sorted in ascending order here for the algorithm to be used correctly. For this you can use 
-- the following functions 'sortFst' and 'sortFstV'. If m >> n than the function gives more efficiency. Even otherwise, 
-- it can be used to simplify the procedure for optimizing the code for transformation of the list data.
-- 
-- b after Vector (a, b) in the type definition of the 'getBFst' must be a defaultValue for 'case' above. 
--
-- Vector (a, b) corresponds to 
--
-- >  a1 -> b1
-- >  a2 -> b2
-- >  a3 -> b3
-- >  ...
-- >  an -> bn
-- 
getBFst :: (Ord a) => V.Vector (a, b) -- ^ Vector of the (a, b) tuples that must be sorted in ascending order for the first argument
  -> b -- ^ a default value that can be substituted if there is no correspendence in the set of (a, b) tuples (the 'otherwise' or irrefutable pattern analogue)
  -> [a] -- ^ a list of values needed to be transformed accordingly to the correct (a, b) tuple pairs
  -> [b] -- ^ the resulting list
getBFst c y = map (getBFst' y c)

-- | Function that sorts a list of (a, b) tuples by the first argument (@a@ must be an instance of class Ord)
-- and is inspired by Data.List.sort function (the last one can be used for sorting the (a, b) tuples where both the types of a and b
-- have instances of the class Ord). It is inspired by the work: https://wiki.haskell.org/Introduction
sortFst :: (Ord a) => [(a, b)] -> [(a, b)]
sortFst xs | not . null $ xs = let z = fst . head $ xs in sortFst (filter (\(x, _) -> x < z) xs) ++ filter (\(x, _) -> x == z) xs ++ sortFst (filter (\(x, _) -> x > z) xs)
           | otherwise = []

-- | Function that prepares the list of (a, b) tuples representing the 
--
-- > case var of 
-- >   a1 -> b1
-- >   a2 -> b2
-- >   a3 -> b3
-- >    ...
-- >   an -> bn
-- >   _  -> defaultValue
--
-- for usage in the 'getBFst' and 'getBFstV' functions. @a@ must be an instance of class Ord.
--
-- The resulting vector has for every @a@ only one element, which was the first in the list of tuples (a, b) after sorting by 'sortFst' function.
--
sortFstV :: (Ord a) => [(a, b)] -> V.Vector (a, b)
sortFstV = V.fromList . L.nubBy (\x y -> fst x == fst y) . sortFst

-- | The function that is used to filter a list [(a, b)] of the corresponding values for getFstB' to obtain the Vector (a, b) 
-- such that the b element for the sequence of pairs (a, b) with the same a is selected by the predicate p and is not necessarily the first one 
-- as it is for the getFstB' function and its successors by default.
filterP :: (Ord a) => ((a, b) -> Bool) -- ^ The predicate p used to select the only one value of b in the pairs (a, b) with the same a. 
  -- ^ If there are several pairs (a, b) for the same a that satisfies a predicate then the first one is used. For large [(a, b)] 
  -- ^ it can be rather complex.
  -> [(a, b)] -- ^ The list of (a, b) sorted in the ascending order by the first element a (e. g. by the 'sortFst' function)
  -> V.Vector (a, b) -- ^ The resulting filtered Vector (a, b) that can be used for getFstB' and its successor functions
--
-- Example: 
--
-- > filterP (\(t, w) -> (t == "1") || (w > 'f')) . sortFst $ [("1",'a'),("4a",'k'),("4a",'b'),("4a",'c'),("4a",'d'),("4a",'e'),("b7",'c'),("b7",'k')] = [("1",'a'),("4a",'k'),("b7",'k')]
-- 
filterP p xs = V.fromList . concatMap (\x -> take 1 . dropWhile (not . p) $ x) $ L.groupBy (\(x1,_) (x2,_) -> x1 == x2) xs
  
  
