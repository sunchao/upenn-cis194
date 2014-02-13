import Data.List

fun1 :: [Integer] -> Integer
fun1 = product . map (flip (-) 2) . filter even

fun2 :: Integer -> Integer
fun2 n = sum . filter even . takeWhile (/= 1) $ iterate
       (\x -> if even x then x `div` 2 else 3*x+1) n


data Tree a = Leaf
          | Node Integer (Tree a) a (Tree a)
            deriving (Show, Eq)

height :: Tree a -> Integer
height Leaf = -1
height (Node h _ _ _) = h


f :: [a] -> Int -> Int -> Tree a
f xs start end  =
    if start > end then Leaf
    else
        let mid = (start + end) `div` 2
            left = f xs start (mid-1)
            right = f xs (mid+1) end
        in Node (1 + (max (height left) (height right))) left (xs !! mid) right

foldTree :: [a] -> Tree a
foldTree xs = f xs 0 (length xs - 1)


xor :: [Bool] -> Bool
xor = foldl (\x y -> x /= y) False

map' :: (a -> b) -> [a] -> [b]
map' f = foldr ((:) . f) []

myfoldl :: (a -> b -> a) -> a -> [b] -> a
myfoldl f base xs = foldr (\b g x -> g (f x b)) id xs base

myfoldl' :: (a -> b -> a) -> a -> [b] -> a
myfoldl' f base xs = foldr step id xs base
    where step x g a = g (f a x)


sieveSundaram :: Integer -> [Integer]
sieveSundaram n = map ((1+) . (2*)) $ [1..n] \\ [i+j+2*i*j | i <- [1..n], j <- [i..n]]