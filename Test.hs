{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}

module Test where

data TestResult = Succeeded | Failed String String

expect :: Show a => a -> (a -> a -> Bool) -> a -> TestResult
expect actual comp target
    | comp actual target = Succeeded
    | otherwise = Failed (show actual) (show target)

type Test = (String, TestResult)

describe :: String -> [String] -> String
describe title tests =
    title ++ ":\n" ++ concatMap ("  " ++) tests

difference :: String -> String -> String
difference str1 str2 =
    first ++ "\n" ++ second where
        (first, second) = unzip (zipWith (\ a t -> (a, if t == a then  ' ' else '👆')) str1 str2)

it :: String -> TestResult -> String
it title res =
    title ++ ": " ++ (
        case res of
            Succeeded -> "✅ OK!"

            Failed actual target -> "❌ failed!\n\ngot:\n" ++ actual ++ "\nbut expected:\n" ++ difference target actual ++ "\n"
    ) ++ "\n"

run :: [String] -> IO ()
run =
    putStrLn . unlines


-- class Test a where
--     expect :: a -> (a -> a -> Bool) -> a -> TestResult

-- expect' a comp b 
--     | comp a b = Succeeded
--     | otherwise = Failed (show a) (show b)

-- instance Test Bool where
--     expect = expect'

-- instance Test [Integer] where
--     expect = expect'

-- instance Test [(String, String)] where
--     expect = expect'