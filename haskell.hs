#!/usr/bin/runhugs 

fact 0 = 1
fact n = n * fact(n-1)

avg ns = sum ns `div` length ns

-- 2.3 --
n = a `div` length xs
    where 
        a = 10
        xs = [1,2,3,4,5]

-- 2.4 -- 
last2 as = last as
last3 as = as !! (length as -1)

-- 2.5 --
init2 as = take (length as -1) as
init3 as = reverse (drop 1 (reverse as))

zeroto :: Int -> [Int]
zeroto n = [0..n]

-- curry -- 
add' :: Int -> (Int -> Int)
add' x y = x + y 

-- pattern matching --
foo ('a',b) = "B"
foo (_,_) = "A" 

-- guarded equations --
signx n | n > 0     = -5
        | n < 0     = 5
        | otherwise = 0

-- 4.1 --
halve as = (take (length as `div` 2) as, drop (length as `div` 2) as) 

-- 4.2 --
--third :: [a] -> a
--third as = head(tail(tail as))
--third as = as !! 3
third [_,_,a] = a

--4.3--
safetail :: [Int] -> [Int]
--safetail as = if length as == 0 then [] else tail as
--safetail as | length as == 0 = []
--            | otherwise = tail as
safetail [] = []
safetail (_:as) = as

--4.4--
orr :: Bool -> Bool -> Bool
False `orr` False = False
_     `orr` _     = True

--4.8--
luhnDouble :: Int -> Int 
luhnDouble n | n > 4     = n*2 `div` 4
             | otherwise = n*2
luhn :: Int -> Int -> Int -> Int -> Bool
luhn a b c d = sum [a,luhnDouble b,c,luhnDouble d] `mod` 10 == 0 

--5--
q51 = sum [ x^2 | x <- [1..100]]

grid :: Int -> Int -> [(Int,Int)]
grid m n = [(x,y) | x <- [0..m], y <- [0..n]]

square :: Int -> [(Int,Int)]
square n = [ t | t <- grid n n, t /= (0,0), t /= (n,n)]

replicate' n a = [a | x <- [1..n]]

pyths :: Int -> [(Int,Int,Int)]
pyths n = [(x,y,z) | x <- [1..n], y <- [1..n], z <- [1..n], x*x + y*y == z*z]

factors :: Int -> [Int]
factors n = [m | m <- [1..n-1], mod n m == 0]

perfects :: Int -> [Int]
perfects n = [m | m <- [1..n], sum (factors m) == m]

xx = [(x,y) | x <- [1,2], y <- [3,4]]
yy = concat [[(x,3),(x,4)] | x <- [1,2]]

find :: Eq k => k -> [(k,v)] -> [v]
find k kv = [v' | (k',v') <- kv, k' == k]

findi :: Eq k => k -> [k] -> [Int]
findi k ks = [i | (k',i) <- zip ks [0..], k' == k]

positions :: Eq a => a -> [a] -> [Int]
-- positions x xs = [i | (x',i) <- zip(xs [0..])] -- , null findi(x' xs)]
-- wrong: -- 
positions x xs = [i | (x',i) <- zip xs [0..], findi x' [xs !! i] == [i]]

scalarproduct :: [Int] -> [Int] -> Int
scalarproduct xs ys = sum [x*y | (x,y) <- zip xs ys]

product' :: Num a => [a] -> a
product' [] = 1
product' (n:ns) = n * product' ns

--6.*--
sumdown :: Int -> Int
sumdown 0 = 0
sumdown n = n + sumdown (n-1)

exp' :: Num n => n -> n -> n
exp' n 0 = 1
exp' n e = n * exp' n (e-1)

euclid :: Int -> Int -> Int
euclid a b | a == b = a
           | a <  b = euclid a (b-a)
           | a >  b = euclid (a-b) b

andb :: [Bool] -> Bool
andb [] = True
andb (True:bs) = andb bs
andb _ = False

concat' :: [[a]] -> [a]
concat' [] = []
concat' (a:as) = a ++ concat' as 

replicate'' :: Int -> a -> [a]
replicate'' 0 _ = []
replicate'' n a = a : replicate' (n-1) a

get_n :: [a] -> Int -> a
get_n (a:as) 0 = a
get_n (a:as) n = get_n as (n-1)

has :: Eq a => a -> [a] -> Bool
has _ [] = False
has a (b:bs) | a == b = True
             | otherwise = has a bs

qsort [] = []
qsort (x:xs) = qsort smaller ++ [x] ++ qsort larger
               where 
                 smaller = [a | a <- xs, a <= x]
                 larger  = [b | b <- xs, b >  x]

merge :: Ord a => [a] -> [a] -> [a]
merge as [] = as
merge [] bs = bs
merge (a:as) (b:bs) | a <= b    = a : merge as (b:bs)
                    | otherwise = b : merge (a:as) bs

--7.x--
filt'  p f xs = [f x | x <- xs, p x]
filt'' p f xs = map f (filter p xs)

all' :: (a -> Bool) -> [a] -> Bool
all' p = and . map p

any' :: (a -> Bool) -> [a] -> Bool
any' p = or . map p

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' _ [] = []
takeWhile' p (a:as) | p a       = a : takeWhile p as
                    | otherwise = []

dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' _ [] = [] 
dropWhile' p (a:as) | p a       = dropWhile' p as
                    | otherwise = a:as 

map' :: (a -> b) -> [a] -> [b]
--map' _ [] = [] 
--map' f (a:as) = f a : map' f as
map' f = foldr (\x xs -> f x : xs) [] 

filt''' :: (a -> Bool) -> [a] -> [a]
--filt''' _ [] = [] 
--filt''' p (a:as) | p a       = a : filt''' p as
--                 | otherwise = filt''' p as
filt''' p = foldr (\a as -> if p a then a:as else as) []

dec2Int :: [Int] -> Int 
dec2Int = foldl (\prev curr -> 10*prev + curr) 0
                 
dec2Intx :: [Int] -> Int 
dec2Intx = foldr (\prev curr -> prev + 10*curr) 0

main = do
         print(scalarproduct [1,2,3] [4,5,6])
         print(product' [4,5.2,6.1])
         print(sumdown 3)
         print(exp' 2 64)
         print(euclid 6 27)
         print(andb [True,True,True])
         print(concat' [[1,2],[3,4]])
         print(replicate'' 5 9)
         print(get_n [5..20] 7)
         print(has (55) [5..20])
         print(qsort [5,2,9,4])
         print(merge [2,3,6] [1,3,4])
         print(filt'' (\n -> mod n 3 == 0) (*10) [0..9])
         print(all' (>1) [0..5])
         print(any' (>1) [0..5])
         print(takeWhile' (<3) [0..6])
         print(dropWhile' (<3) [0..6])
         print(map' (*10) [0..6])
         print(filt''' (\n -> n `mod` 3 == 0) [0..6])
         print(dec2Int [1,2,3,5])
         print(dec2Intx [1,2,3,5])

