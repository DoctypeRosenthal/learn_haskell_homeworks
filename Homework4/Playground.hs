module Playground where

compose :: (b -> c) -> (a -> b) -> (a -> c)
compose g f = g . f

foobar' :: [Integer] -> Integer
foobar' = sum . map ((+2) . (*7)) . filter (>3)

main :: IO()
main =
    print (foobar' [1,2,3,4,5,6])