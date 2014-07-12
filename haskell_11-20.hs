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
decode :: [Result a] => [a]
decode = foldl (\xs result -> xs ++ (helper result)) []
  where helper (Single x)      = [x]
        helper (Multiple 1 x)  = [x]
        helper (Multiple i x)  = x : helper (Multiple (i - 1) x)

decode' :: [Result a] => [a]  -- Revisited using concatMap
decode' = concatMap decoder
  where decoder (Single x)      = [x]
        decoder (Multiple n x)  = replicate n x
