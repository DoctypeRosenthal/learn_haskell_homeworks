module Golf where
import Test 
import Data.List
import qualified Data.Tuple as Tuple
import Text.Printf
import Debug.Trace

skips :: [b] -> [[b]]
skips [] = []
skips xs = map skipN $ zip [1..] $ filter (not.null) $ tails xs 

skipN :: (Integer, [b]) -> [b]
skipN (n, list) =
    map Tuple.snd $ filter everyNthElement $ zip [0..] list where
        everyNthElement (i, _) = i `mod` n == 0

-- EX 2

maxOfThree :: [Integer] -> [Integer]
maxOfThree (left:mid:right:_) = 
    if left <= mid && right <= mid 
        then [mid] 
        else []
maxOfThree _ = []

localMaxima :: [Integer] -> [Integer]
localMaxima xs = 
    concatMap maxOfThree.(take 3) $ tails xs

-- EX3

type Quantity = Int
type NumQuantity = (Integer, Quantity)

nums :: [Integer]
nums = [0..9]

numbersAndQuantities :: [NumQuantity]
numbersAndQuantities =
    zip nums (repeat 0)

padLeft :: Int -> String -> String
padLeft count str 
    | count <= length str = str
    | otherwise = replicate (count - length str) ' ' ++ str

showIn2D ::  [NumQuantity] -> String
showIn2D xs = 
    concat $ intersperse "\n" $ paddedStrings where
        starRepresentation :: [String]
        starRepresentation = 
            map (\(i, count) -> replicate count '*' ++ "=" ++ show i) xs
        
        maxLen = 
            length $ maximum starRepresentation
        
        paddedStrings :: [String]
        paddedStrings = 
            map (padLeft maxLen) starRepresentation


findAndIncreaseNum :: [NumQuantity] -> Integer -> [NumQuantity]
findAndIncreaseNum acc num =
    map (\item@(i, count) -> if i == num then (i, count + 1) else item) acc

histogram :: [Integer] -> String
histogram list = 
    showIn2D $ foldl findAndIncreaseNum numbersAndQuantities list
        

tests :: [String]
tests =
    [describe "Golf"
        [ it "1" $ expect (skips "ABCD") (==) ["ABCD", "BD", "C", "D"] 
        , it "2" $ expect (skips "hello!") (==) ["hello!", "el!", "l!", "l", "o", "!"]
        , it "3" $ expect (skips [1]) (==) [[1]]
        , it "4" $ expect (skips [True,False]) (==) [[True,False], [False]]
        , it "5" $ expect (skips ([] :: [()]) ) (==) []
    
        , it "6" $ expect (localMaxima [2,9,5,6,1]) (==) [9,6]
        , it "7" $ expect (localMaxima [2,3,4,1,5]) (==) [4]
        , it "8" $ expect (localMaxima [1,2,3,4,5]) (==) []

        , it "9" $ expect (histogram [1,1,1,5]) (==) 
            " *        \n\
            \ *        \n\
            \ *   *    \n\
            \==========\n\
            \0123456789\n"
        ]
    ]

-- >>> bla
-- Variable not in scope: bla
main :: IO()
main =
    run tests