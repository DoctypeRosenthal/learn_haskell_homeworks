{-# LANGUAGE FlexibleInstances #-}

module Main where
import Utils
import Data.Char
import Debug.Trace (trace)

digitToInteger = toInteger . digitToInt

toDigits :: Integer -> [Integer]
toDigits n
    | n <= 0 = []
    | otherwise = map digitToInteger $ show n  

toDigitsRev :: Integer -> [Integer]
toDigitsRev = reverse . toDigits

doubleTheSecond :: Integer -> [Integer] -> [Integer]
doubleTheSecond x ints =
    if length ints == 0 then
        [x]
    else if isEven $ length ints then 
        x : ints
    else
        x*2 : ints
        
    
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther list =
    foldr doubleTheSecond [] list 


sumDigits :: [Integer] -> Integer
sumDigits =
    sum . flatmap . map toDigits
         
validate :: Integer -> Bool
-- validate int | trace (show (mod ((sumDigits . doubleEveryOther . toDigits) int) 10)) False = undefined
validate int =
    dividableBy10 $ (sumDigits . doubleEveryOther . toDigits) int
    where 
        dividableBy10 number = 0 == mod number 10

-- EXCERSIZE 5

type Peg = String
type Move = (Peg, Peg)

{- 
Perform game Tower of Hanoi.
Move a stack of discs ordered in size from first peg to second.
 -}
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]

hanoi 0 _ _ _ =
    []

hanoi n "a" "b" "c" =
    ("a", "c") : hanoi n "a" "c" "b"

hanoi n "a" "c" "b" =
    ("a", "b") : hanoi (n-1) "c" "a" "b"

hanoi n "c" "a" "b" =
    ("c", "b") : hanoi (n-1) "a" "b" "c"




-- TESTS
runTests :: IO ()
runTests =
    putStrLn $ makeTestResultsReadable
        [ ("1", expect (toDigits 1234) (==) [1,2,3,4])
        , ("2", expect (toDigitsRev 1234) (==) [4,3,2,1])
        , ("3", expect (toDigits 0) (==) [])
        , ("4", expect (toDigits (-17)) (==) [])
        , ("5", expect (doubleEveryOther [8,7,6,5]) (==) [16,7,12,5])
        , ("6", expect (doubleEveryOther [1,2,3]) (==) [1,4,3])
        , ("7", expect (validate 4012888888881881) (==) True)
        , ("8", expect (validate 4012888888881882) (==) False)
        , ("hanoi", expect (hanoi 2 "a" "b" "c") (==) [("a","c"), ("a","b"), ("c","b")])
        ]

main :: IO ()
main = 
    runTests
        