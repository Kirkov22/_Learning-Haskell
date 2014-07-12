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
   funciton.
-}
