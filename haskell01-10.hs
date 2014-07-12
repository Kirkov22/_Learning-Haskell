{-  Exercises 01-10 from 99 Haskell Problems
    http://www.haskell.org/haskellwiki/H-99:_Ninety-Nine_Haskell_Problems
-}

import Data.List -- used for #8, group function

--  Problem 01: Last element of a list
myLast :: [a] -> a
myLast []     = error "Can't call myLast on an empty list!"
myLast (x:[]) = x
myLast (_:xs) = myLast xs

-- Problem 02: Second to last element of a list
myButLast :: [a] -> a
myButLast list
  | length list < 2 = error "Not enough elements"
  | otherwise       = element list
  where element (x:_:[])  = x
        element (_:x)     = myButLast x

-- Problem 03: Find the Kth element of a list
elementAt :: [a] -> Int -> a
elementAt list index = list !! (index - 1)

-- Problem 04: Find the number of elements in a list
myLength :: [a] -> Int
myLength []     = 0
myLength (_:xs) = 1 + myLength xs

myLength' = foldl (\n _ -> n + 1) 0 -- Revisited using foldl

-- Problem 05: Reverse a list
myReverse :: [a] -> [a]
myReverse [] = []
myReverse xs = (last xs) : (myReverse (init xs))

myReverse' = foldl (\a x -> x : a) [] -- Revisited using foldl

-- Problem 06: Determine if a list is a palindrome
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome []   = True
isPalindrome [_]  = True
isPalindrome xs   = ((head xs) == (last xs)) && (isPalindrome (tail (init xs)))

-- Problem 07: Flatten a nested list structure
data NestedList a = Elem a | List [NestedList a]

flatten :: NestedList a -> [a]
flatten (Elem x)      = [x]
flatten (List [])     = []
flatten (List (x:xs)) = flatten x ++ flatten (List xs)

-- Problem 08: Eliminate consecutive duplicates of list elements
compress :: (Eq a) => [a] -> [a]
compress []       = []
compress [x]      = [x]
compress (x:y:xs) =
  if (x == y)
    then compress(y:xs)
    else x : (compress(y:xs))

compress' :: (Eq a) => [a] -> [a] -- Revisited using map & Data.List module
compress' = map head . group

-- Problem 09: Pack conservative duplicates of list elements into sublists
pack :: (Eq a) => [a] -> [[a]]
pack [] = [[]]
pack [x] = [[x]]
pack list@(x:xs) =
  if (x == head (head $ pack xs))
    then (x:(head $ pack xs)):(tail $ pack xs)
    else [x]:(pack xs)

-- Problem 10: Encode duplicates using solution to problem 09
encode :: (Eq a) => [a] -> [(Int, a)]
encode = map (\l -> (length l, head l)) . pack
