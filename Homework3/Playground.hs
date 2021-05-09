module Playground where

data IntList = Empty | List Int IntList 
    deriving Show

mapIntList :: (Int -> Int) -> IntList -> IntList
mapIntList _ Empty = Empty
mapIntList fn (List x xs) = 
    List (fn x) (mapIntList fn xs)

filterIntList :: (Int -> Bool) -> IntList -> IntList
filterIntList _ Empty = Empty
filterIntList fn (List x xs)
    | fn x = List x filteredRest
    | otherwise = filteredRest
    where 
        filteredRest = filterIntList fn xs

foldlIntList :: (Int -> Int -> Int) -> Int -> IntList -> Int
foldlIntList _ acc Empty = acc
foldlIntList fn acc (List x xs) = 
    foldlIntList fn (fn x acc) xs

exampleList :: IntList
exampleList = 
    List (-1) (List 2 (List (-6) Empty))

-- polymorphic functions

mapList :: (a -> b) -> [a] -> [b]
mapList _ [] = []
mapList fn (x:xs) = (fn x) : mapList fn xs

isEven :: Integer -> Bool
isEven int =
    int `mod` 2 == 0

-- contains :: String -> String -> Bool
-- contains "" "" = True
-- contains (x:xs) "" = False
-- contains "" (x:xs) = False
-- contains needle haystack@(x:xs) =
--     letters == needle || contains needle xs where
--         letters = take (length needle) haystack 
-- Gibt's schon: isInfixOf und isSubsequenceOf

filterList :: (a -> Bool) -> [a] -> [a]
filterList _ [] = []
filterList fn (x:xs)
    | fn x = x : filterList fn xs
    | otherwise = filterList fn xs


reduceList :: (a -> b -> b) -> b -> [a] -> b
reduceList _ acc [] = acc
reduceList fn acc (x:xs) = 
    reduceList fn (fn x acc) xs
