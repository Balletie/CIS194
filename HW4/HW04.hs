{-# OPTIONS_GHC -Wall #-}
module HW04 where

import BST
import Data.Char
import Data.Maybe
import Data.List

-- |The seq function also has this type.
ex1 :: a -> b -> b
ex1 _ y = y

ex2 :: a -> a -> a
ex2 x _ = x

ex3 :: Int -> a -> a
ex3 x y = ex1 x y

-- |False selects the first argument, True selects the second argument.
-- The Bool argument gives a way to select which argument to use.
ex4 :: Bool -> a -> a -> a
ex4 False x _ = x
ex4 True _ y = y

-- |Could also have used the id function.
ex5 :: Bool -> Bool
ex5 False = True
ex5 _ = False

-- |Not possible because the function passed needs an argument, and
-- and we can't know what the type is of the argument.
ex6 :: (a -> a) -> a
ex6 = error "impossible!"

-- |Given a function and a parameter for that function,
-- apply the parameter.
ex7 :: (a -> a) -> a -> a
ex7 f x = f x

-- |Any list operation can satisfy, or the id function.
ex8 :: [a] -> [a]
ex8 = reverse

-- |Map a function over a list.
ex9 :: (a -> b) -> [a] -> [b]
ex9 = map

-- |Cannot be total, because if the value is Nothing, then we can't
-- be sure what to return.
ex10 :: Maybe a -> a
ex10 (Just x) = x
ex10 Nothing = error "impossibe!"

-- |Another possibility is to construct Nothing, disregarding the parameter.
ex11 :: a -> Maybe a
ex11 x = Just x

-- |This is essentially the same as the identity function.
-- An alternative is to return Nothing on all inputs.
ex12 :: Maybe a -> Maybe a
ex12 = id

-- |Insert the element (second parameter) in the BST (third parameter)
-- using the function giving the Ordering (first parameter)
insertBST :: (a -> a -> Ordering) -> a -> BST a -> BST a
insertBST _ val Leaf = Node Leaf val Leaf
insertBST comp val (Node left x right) = insert_dir (val `comp` x)
    where insert_dir LT = Node (insertBST comp val left) x right
          insert_dir _  = Node left x (insertBST comp val right)

safeHead :: [a] -> Maybe a
safeHead []     = Nothing
safeHead (x:_)  = Just x

-- |Exercise 14: Check if a List of Strings contains only capitalized
-- words.
allCaps :: [String] -> Bool
allCaps []     = True
allCaps (x:xs) = upperCase x && allCaps xs

upperCase :: String -> Bool
upperCase []    = False
upperCase (x:_) = isUpper x

-- |Exercise 15: Drop trailing whitespaces from a string
dropTrailingWhitespace :: String -> String
-- Is this efficient enough?
dropTrailingWhitespace = (reverse . dropWhile (== ' ') . reverse)

-- |Exercise 16: Get the first letter of every string in the list
firstLetters :: [String] -> [Char]
firstLetters = mapMaybe safeHead

-- |Exercise 17: Render a List of Strings to a String representation
asList :: [String] -> String
asList x = "[" ++ (concat (intersperse "," x)) ++ "]"
