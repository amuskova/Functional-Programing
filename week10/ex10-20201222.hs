--task 1 fibonacci

--fibonacci :: Int -> Int

import Data.List (nub,sortOn,maximumBy,isPrefixOf)
import Data.Ord (comparing)
import Data.Maybe (isJust, isNothing)


fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n-1) + fibonacci (n-2)


--task 3 fastPow

fastPow _ 0 = 1
fastPow x 1 = x
fastPow x n 
   | even n    = half * half
   | otherwise = half * half * x
   where half = fastPow x (n `div` 2)


--task 4 complAdd, complSub, complMul

complAdd (x,y) (z,d) = (x + z, y + d)
complSub (x,y) (z,d) = (x - z, y - d)
complMul (x,y) (z,d) = (x * z - y * d , x * d + y * z)

--task 5 distance

distance (x,y) (z,d) = sqrt((z - x)^2 + (d - y)^2)

--task 6 repeated
repeated _ 0 = \x -> x
repeated f n = \x -> f ((repeated f (n - 1)) x)

---------------------------------------------------------------------
--ex11

--task1
--use foldl and foldr for minimum'/maximum', reverse', all'/any', append', replicate'

--minimum'/maximum'
minimum' (x:xs) = foldl min x xs
maximum' (x:xs) = foldl max x xs

--reverse'
reverse' lst = foldl(\ res el -> el:res) [] lst

--all'/any'

all' p lst = foldr (\ es res -> p es && res) True lst
any' p lst = foldr (\ es res -> p es || res) True lst

--append' 
append' lst1 lst2 = foldr (:) lst2 lst1
append'' lst1 lst2 = foldr (:) lst1 lst2

--replicate'
replicate' n x = foldr(\ _ res -> x:res) [] [1..n]


--task2

countDivisors n = length [ d | d <- [2..n-1], n `mod` d == 0]
prime n = (countDivisors n) == 0
descartes lst1 lst2 = [(x,y) | x <- lst1, y <- lst2 ]

--task3

primes = filter prime [2..]

--task4 

primes' = sieve [2..]
  where sieve (x:xs) = x : sieve [ y | y<-xs, y `mod` x/= 0 ]

--task5

pairs = [ (d-i, i) | d<-[0..], i<-[0..d]]

--task6

pyths = [ (a,b,c) | c <- [5..], b <- [1..c-1], a<-[1..b-1], a^2 + b^2 == c^2]

--task7

compress [] = []
compress lst = (head lst, length heads) : compress rest
   where (heads, rest) = span (\x -> x == head lst) lst

--task8 

maxRepeated lst = maximum [ n | (_,n) <- compress lst]

--task9 

makeSet lst = foldr (\ el res -> if el `elem` res then res else el:res) [] lst

--task10

histogram lst = [ (el,count el) | el<-makeSet lst ]
     where count el = length [ x | x<-lst, x == el]

--task11

maxDistance pts = maximum [ dst p1 p2 | p1<-pts, p2<-pts ]
   where dst (x1, y1) (x2, y2) = sqrt $ (x1-x2)^2 + (y1-y2)^2







--task12
mostFrequent :: (Eq a) => [a] -> a
mostFrequent xs = fst $ maximumBy (comparing snd) histo 
    where histo = histogram xs

specialSort :: (Eq a, Ord a) => [[a]] -> [[a]]
specialSort xss = sortOn mostFrequent xss


------------------------------------------
--ex12

--task1
data Parity = Even | Odd

instance Eq Parity where
    Even == Even = True
    Odd == Odd = True
    _==_ = False

parity :: Int->Parity
parity x = if even x then Even else Odd

isEven :: Parity -> String
isEven Even = "ieii"
isEven _ = "noo"

data IntTree = Empty | Node Int IntTree IntTree -- nalagame organicheniq

treeSum :: IntTree -> Int
treeSum Empty = 0
treeSum (Node val left right) = val + treeSum left + treeSum right


--task3 safeHead, safeTail, safeUncons, findIndex, stripPrefix, maybeToList, mapMaybe

--safeHead
safeHead :: [a] -> Maybe a
safeHead []  = Nothing
safeHead (x:_) = Just x

--safeTail
safeTail :: [a] -> Maybe [a]
safeTail [] = Nothing
safeTail (_:xs) = Just xs

--safeUncons
safeUncons :: [a] -> Maybe (a, [a])
safeUncons [] = Nothing
safeUncons (x:xs) = Just (x,xs)

--findIndex
findIndex :: Eq a => a -> [a] -> Maybe Int
findIndex _ [] = Nothing
findIndex x (y:ys)
 |x == y    = Just 0
 | (Just idx) <- findIndex x ys = Just (1 + idx)
 | otherwise = Nothing

--stripPrefix
stripPrefix :: Eq a => [a] -> [a] -> Maybe [a]
stripPrefix lst1 lst2
  |lst1 `isPrefixOf` lst2 = Just $ drop (length lst1) lst2
  | otherwise = Nothing

--maybeToList
maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just x) = [x]

--mapMaybe
mapMaybe :: (a->b) -> Maybe a -> Maybe b
mapMaybe _ Nothing = Nothing
mapMaybe f (Just x) = Just (f x)

---------
data Tree a = Empty | Node a (Tree a) (Tree a)
testTree = Node 5 (Node 6 Empty Empty) (Node (-10) (Node 2 Empty Empty) Empty)


--task5 

maxSumPath :: (Num a, Ord a) => Tree a -> a
maxSumPath Empty = 0
maxSumPath (Node value left right) = val + max (maxSumPath left) (maxSumPath right)

--task6

prune :: Tree a -> Tree a
prune Empty = Empty
prune (Node _ Empty Empty) = Empty
prune (Node value left right) = Node value (prune left) (prune right)

--task7
bloom :: Tree a -> Tree a
bloom Empty = Empty
bloom (Node x Empty Empty) = Node value (Node value Empty Empty) (Node value Empty Empty)
bloom (Node value left right) = Node value (bloom left) (bloom right) 

--task8
rotateLeft :: Tree a -> Tree a
rotateLeft Empty = Empty
rotateLeft (Node value left (Node q b c)) = Node q (Node value left b) c
rotateLeft t = t

rotateRight :: Tree a -> Tree a
rotateRight (Node q (Node p a b) c) = Node p a (Node ` b c)
rotateRight t = t

--task9 
treeMap :: Tree a -> Tree a
treeMap _ Empty = Empty
treeMap f (Node value left right) = Node (f value) (treeMap f left) (treeMap f right)

--task10

instance Functor Tree where
     fmap = treeMap

--task11

data BST a = BSTEmpty | BSTNode a (BST a) (BST a)
-------------------

--Graphs

type Graph = [[Int]]

testGraph :: Graph
testGraph = [[1,2], [2,5], [3], [5],[], [4]]

graphSize :: Graph -> Int
graphSize = length

neighbs :: Int -> Graph -> [Int]
neighbs u g = g !! u

data Color = White | Gray | Black deriving (Show)
type State = [Color]

update :: Int -> a -> [a] -> [a]
update idx val lst = (take idx lst) ++ (val : drop (idx+1) lst)

dfs g = foldl helper (replicate n White) [0..n-1]
    where n = graphSize g
          helper :: State -> Int -> State
          helper colors u = case colors !! u of White -> dfsVisit u colors
                                                      -> colors
          dfsVisit :: Int -> State -> State
          dfsVisit u colors = let colors' = foldl helper