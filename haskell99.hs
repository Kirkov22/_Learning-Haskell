{-  Exercises from 99 Haskell Problems
    http://www.haskell.org/haskellwiki/H-99:_Ninety-Nine_Haskell_Problems
-}

--  Problem One: Last element of a list
myLast :: [a] -> a
myLast []     = error "Can't call myLast on an empty list!"
myLast (x:[]) = x
myLast (_:xs) = myLast xs

-- Problem Two: Second to last element of a list
myButLast :: [a] -> a
myButLast list
  | length list < 2 = error "Not enough elements"
  | otherwise       = element list
  where element (x:_:[])  = x
        element (_:x)     = myButLast x

-- Problem Three: Find the Kth element of a list
elementAt :: [a] -> Int -> a
elementAt list index = list !! (index - 1)

-- Problem Four: Find the number of elements in a list
myLength :: [a] -> Int
myLength [] = 0
myLength (_:xs) = 1 + myLength xs
