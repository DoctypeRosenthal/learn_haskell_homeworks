import Test

fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
    | even x = (x - 2) * fun1 xs
    | otherwise = fun1 xs

fun1' :: [Integer] -> Integer
fun1' = product . map (subtract 2) . filter even


fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n 
    | even n = n + fun2 (n `div` 2)
    | otherwise = fun2 (3 * n + 1)

fun2' :: Integer -> Integer
fun2' _ =
    0
    
tests :: [String]
tests =
    [describe "Golf"
        [ it "creates the same result (I)" $ expect (fun1 [1,2,3,4,5,6]) (==) (fun1' [1,2,3,4,5,6])
        , it "creates the same result (II)" $ expect (fun2 10) (==) (fun2' 10)
        ]
    ]

main :: IO()
main =
    run tests