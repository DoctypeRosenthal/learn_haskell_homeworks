{-# LANGUAGE RankNTypes #-}
import Debug.Trace
import Test

-- EX1

fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x : xs)
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
-- fun2' n | trace ("fun2':  " ++ show (sum . filter even . takeWhile (/= 1) . iterate (\x -> if even x then x `div` 2 else 3*x + 1) $ n)) False = undefined
fun2' =
  sum . filter even . takeWhile (/= 1) . iterate (\x -> if even x then x `div` 2 else 3 * x + 1)

-- EX2

data Tree a
  = Leaf
  | Node Integer (Tree a) a (Tree a)
  deriving (Show, Eq, Ord)

foldTree :: (Ord a, Show a) => [a] -> Tree a
foldTree lst | trace ("foldTree:  " ++  (visualize  $ balance $ foldr (insertAsLeaf . toNode) Leaf lst)) False = undefined
-- foldTree lst = 
--   foldr (insertAsLeaf . toNode) Leaf lst

toNode :: a -> Tree a
toNode x =
  Node 0 Leaf x Leaf

treeHeight :: Tree a -> Integer
treeHeight Leaf = 0
treeHeight (Node height Leaf _ Leaf) = height
treeHeight (Node _ Leaf _ right) = treeHeight right
treeHeight (Node _ left _ Leaf) = treeHeight left
treeHeight (Node _ left _ right) = max (treeHeight left) (treeHeight right)

setNodeHeight :: Tree a -> Integer -> Tree a
setNodeHeight Leaf _ = Leaf
setNodeHeight (Node _ l v r) nextHeight =
  Node nextHeight (setNodeHeight l (nextHeight + 1)) v (setNodeHeight r (nextHeight + 1))

insertLeafRight :: (Ord a, Show a) => Tree a -> Tree a -> Tree a
insertLeafRight (Node height _ valCurr _) (Node _ _ valNext _) | trace ("insert " ++ show valNext ++ " right of " ++ show valCurr ++ " with new height " ++ show (height + 1)) False = undefined
insertLeafRight (Node height l v Leaf) nextNode =
  Node height l v (setNodeHeight nextNode $ height + 1)
insertLeafRight (Node height l v r) nextNode =
  Node height l v (insertAsLeaf r nextNode)

insertLeafLeft :: (Ord a, Show a) => Tree a -> Tree a -> Tree a
insertLeafLeft (Node height _ valCurr _) (Node _ _ valNext _) | trace ("insert " ++ show valNext ++ " left of " ++ show valCurr ) False = undefined
insertLeafLeft (Node height Leaf v r) nextNode =
  Node height (setNodeHeight nextNode $ height + 1) v r
insertLeafLeft (Node height l v r) nextNode =
  Node height (insertAsLeaf l nextNode) v r

insertAsLeaf :: (Ord a, Show a) => Tree a -> Tree a -> Tree a
insertAsLeaf _ Leaf | trace ("\ninsertNode: Leaf!" ) False = undefined
insertAsLeaf _ (Node _ _ v _) | trace ("\ninsertNode: " ++ show v ) False = undefined
insertAsLeaf Leaf node = node -- EVENTUELL FEHLER????? <-------------------------
insertAsLeaf currentNode nextNode
  | currentNode < nextNode =  insertLeafRight currentNode nextNode
  | otherwise = insertLeafLeft currentNode nextNode


leftIsHeavier :: Tree a -> Bool
leftIsHeavier (Node height l v r) =
  treeHeight l - treeHeight r > 1

rightIsHeavier :: Tree a -> Bool
rightIsHeavier (Node height l v r) =
  treeHeight l - treeHeight r < -1

balance :: (Ord a, Show a) => Tree a -> Tree a
balance Leaf | trace ("\nrotate: Leaf!") False = undefined
balance Leaf = Leaf
balance tree@(Node _ Leaf _ Leaf) = tree
balance tree@(Node _ l _ r) | trace ("\nrotate: treeBalance: " ++ show ( treeHeight l - treeHeight r) ) False = undefined
balance tree@(Node height l v r)
  | leftIsHeavier tree =
    balance $ rotateRight tree
  | rightIsHeavier tree =
    balance $ rotateLeft tree
  | otherwise =
    -- tree
    Node height (balance l) v (balance r) -- WAS IST HIER FALSCH???
    -- where 
      -- withBalancedBranches = Node height (balance l) v (balance r) -- WAS IST HIER FALSCH???


rotateLeft :: (Ord a, Show a) => Tree a -> Tree a
rotateLeft this@(Node height l v r@(Node _ rLeft rValue rRight)) =
  -- right child becomes parent
  setNodeHeight (Node 0 newLeft rValue rRight) height where
    newLeft =
       -- this node becomes new Left and inherits right's left node as the new right node
      Node 0 l v rLeft

rotateRight :: (Ord a, Show a) => Tree a -> Tree a
rotateRight this@(Node height l@(Node _ lLeft lValue lRight) v r) =
  -- right child becomes parent
  setNodeHeight (Node 0 lLeft lValue newRight) height where
    newRight =
       -- this node becomes new right and inherits left's right node as it's left node
      Node 0 lRight v r



visualize :: Show a => Tree a -> String
visualize Leaf = ""
visualize (Node height l v r) =
  "\n" ++ show v ++ show height ++
  (visualize l ++ "\t" ++ visualize r)


tests :: [String]
tests =
  [ describe
      "Homework4: Composition, idiomatic programming in Haskell"
      [ it "creates the same result (I)" $ expect (fun1' [1, 2, 3, 4, 5, 6]) (==) (fun1 [1, 2, 3, 4, 5, 6])
      -- , it "creates the same result (II)" $ expect (fun2' 10) (==) (fun2 10)
      -- , it "creates the same result (III)" $ expect (fun2' 11) (==) (fun2 11)
      -- , it "creates the same result (IV)" $ expect (fun2' 100) (==) (fun2 100)
      -- , it "creates the same result (V)" $ expect (fun2' 1) (==) (fun2 1)
      , it "creates a binary tree" $
          expect
            (foldTree "ABCDEF")
            (==)
            ( Node
                3
                ( Node
                    2
                    (Node 0 Leaf 'F' Leaf)
                    'I'
                    (Node 1 (Node 0 Leaf 'B' Leaf) 'C' Leaf)
                )
                'J'
                ( Node
                    2
                    (Node 1 (Node 0 Leaf 'A' Leaf) 'G' Leaf)
                    'H'
                    (Node 1 (Node 0 Leaf 'D' Leaf) 'E' Leaf)
                )
            )
      ]
  ]

main :: IO ()
main =
  run tests
