{-  Exercises 21-28 from 99 Haskell Problems
    http://www.haskell.org/haskellwiki/H-99:_Ninety-Nine_Haskell_Problems
-}

-- Problem 21: Insert an element into a list at position n
insertAt :: a -> [a] -> Int -> [a]
insertAt element list index
  | index > length list = error "Index out of range"
  | index < 0 = insertAt element list (index + length list)
  | otherwise = pre ++ (element : post)
  where
    (pre, post) = splitAt index list
