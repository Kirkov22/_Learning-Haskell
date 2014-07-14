{-  Exercises 11-20 from 99 Haskell Problems
    http://www.haskell.org/haskellwiki/H-99:_Ninety-Nine_Haskell_Problems
-}

import Data.List  -- used for # 10, group function

-- Problem 10: Encode duplicates using solution to problem 09
-- Needed for Problem 11
encode :: (Eq a) => [a] -> [(Int, a)]
encode = map (\l -> (length l, head l)) . group

-- Problem 11: Modified encoding using a Single/Multiple datatype
data Result a = Single a | Multiple Int a
instance (Show a) => Show (Result a) where
  show (Single a) = "Single " ++ (show a)
  show (Multiple x a) = "Multiple " ++ (show x) ++ ' ':(show a)

encodeModified :: (Eq a) => [a] -> [Result a]
encodeModified = map modifier . encode
  where modifier (1, x) = Single x
        modifier (n, x) = Multiple n x

-- Problem 12: Decode the result of modified encoding from #11
decode :: [Result a] -> [a]
decode = foldl (\xs result -> xs ++ (helper result)) []
  where helper (Single x)      = [x]
        helper (Multiple 1 x)  = [x]
        helper (Multiple i x)  = x : helper (Multiple (i - 1) x)

decode' :: [Result a] -> [a]  -- Revisited using concatMap
decode' = concatMap decoder
  where decoder (Single x)      = [x]
        decoder (Multiple n x)  = replicate n x

-- Problem 13: Directly encode a list using Single/Multiple datatypes
increment :: Result a -> Result a
increment (Single x)      = Multiple 2 x
increment (Multiple n x)  = Multiple (n + 1) x

value :: Result a -> a
value (Single x)      = x
value (Multiple n x)  = x

encodeDirect :: (Eq a) => [a] -> [Result a]
encodeDirect = foldr helper []
  where
    helper element list
      | null list                       = [Single element]
      | (value . head) list == element  = (increment . head) list : (tail list)
      | otherwise                       = Single element : list

{- Upon reviewing the solution for this problem, it looks like I misunderstood the
   constraint of 'directly' computing the encoding. The official solution computes
   a list of tuples ie (n, x) and then converts that list of tuples into the
   Single/Mutiple data type.

   My solution does this all in one pass through the list using an extra helper
   function.
-}

-- Problem 14: Duplicate the elements of a list
dupli :: [a] -> [a]
dupli []      = []
dupli (x:xs)  = x:x:(dupli xs)

-- Problem 15: Replicate the elements of a list n times
repli :: [a] -> Int -> [a]
repli list n = concat [ replicate n x | x <- list]

-- Problem 16: Drop every nth element of a list
dropEvery :: [a] -> Int -> [a]
dropEvery xs n = dropEvery' xs n 1

dropEvery' :: [a] -> Int -> Int -> [a]
dropEvery' []          _ _    = []
dropEvery' list@(x:xs) n sum  | mod sum n == 0  = dropEvery' xs n (sum + 1)
                              | otherwise       = x : dropEvery' xs n (sum + 1)

-- Problem 17: Split a list into two pieces given a size for the first
split :: [a] -> Int -> ([a], [a])
split []          _ = ([], [])
split list@(x:xs) n | n > 0     = (x : ys, zs)
                    | otherwise = ([], list)
  where (ys, zs) = split xs (n - 1)

-- Problem 18: Take a slice of the ith through kth element in a list
--   Starting index = 1
slice :: [a] -> Int -> Int -> [a]
slice list start stop = slice' list start stop 1
  where  
    slice' []          _     _    _     = []
    slice' list@(x:xs) start stop index 
      | index > stop = []
      | index >= start = x : slice' xs start stop  (index + 1)
      | otherwise = slice' xs start stop (index + 1)

-- Problem 19: Rotate a list n places to the left
rotate :: [a] -> Int -> [a]
rotate [] _ = []
rotate l  0 = l
rotate list@(x:xs) n
  | n < 0 = rotate list (length list + n)
  | otherwise = rotate (xs ++ [x]) (n - 1)
