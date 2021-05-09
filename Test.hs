{-# LANGUAGE TypeFamilies #-}

module Test where


data TestResult = Succeeded | Failed String String


expect :: Show a => a -> (a -> a -> Bool) -> a -> TestResult
expect actual comp target 
    | comp actual target = Succeeded
    | otherwise = Failed (show actual) (show target)

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

type Test = (String, TestResult)

describe :: String -> [String] -> String
describe title tests =
    title ++ ":\n" ++ concatMap ((++) "  ") tests

it :: String -> TestResult -> String
it title res =
    title ++ ": " ++ (
        case res of
            Succeeded -> "✅ success! \033[1;31m This is red text"

            Failed actual target -> "❌ got '" ++ actual ++ "' but expected '" ++ target ++ "'." 
    ) ++ "\n" 

run :: [String] -> IO ()
run =
    putStrLn . unlines