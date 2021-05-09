-- {-# LANGUAGE FlexibleInstances #-}

module Utils where


class WholeNum n where
    isEven :: n -> Bool


isEven' n = mod n 2 == 0

instance WholeNum Int where
    isEven = isEven'

instance WholeNum Integer where
    isEven = isEven'


flatmap :: [[a]] -> [a]  
flatmap [] = []  
flatmap (x:xs) = x ++ flatmap xs

