{-  Exercises from 99 Haskell Problems
    http://www.haskell.org/haskellwiki/H-99:_Ninety-Nine_Haskell_Problems
-}

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

-- Problem 05: Reverse a list
myReverse :: [a] -> [a]
myReverse [] = []
myReverse xs = (last xs) : (myReverse (init xs))

-- Problem 06: Determine if a list is a palindrome
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome [] = True
isPalindrome [_] = True
isPalindrome xs = ((head xs) == (last xs)) && (isPalindrome (tail (init xs)))
