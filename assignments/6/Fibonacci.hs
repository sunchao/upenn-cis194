{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}

fib :: Integer -> Integer
fib n
    | n == 0  = 0
    | n == 1  = 1
    | otherwise = fib(n-1) + fib(n-2)


fibs1 :: [Integer]
fibs1 = [ fib(x) | x <- [0..] ]

fibs2 :: [Integer]
fibs2 = [0,1] ++ zipWith (+) fibs2 (tail fibs2)


-- Exercise 3

data Stream a = Cons a (Stream a)


streamToList :: Stream a -> [a]
streamToList (Cons a as) = a : streamToList as

instance Show a => Show (Stream a) where
    show s = "[" ++ (concat . map f $ take 20 $ streamToList s) ++ "...]"
             where f x = (show x) ++ ","

-- Exercise 4

-- Generates a stream containing infinitely many copies
-- of the given element
streamRepeat :: a -> Stream a
streamRepeat n = Cons n (streamRepeat n)

-- Applies a function to every element of a Stream
streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons a xs) = Cons (f a) $ streamMap f xs

-- Generates a Stream from a "seed" of type a, which is the
-- first element of the stream, and an "unfolding rule" of type
-- a -> a which specifies how to transform the seed into a new
-- seed, to be used for generating the rest of the stream.
streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f a = Cons a $ streamFromSeed f (f a)

-- Exercise 5

nats :: Stream Integer
nats = streamFromSeed (1+) 0

interleaveStreams :: Stream Integer -> Stream Integer -> Stream Integer
interleaveStreams (Cons a as) bs = Cons a $ interleaveStreams bs as
-- interestingly, the following doesn't work, and will cause ruler to
-- be non-terminate
-- interleaveStreams (Cons a as) (Cons b bs) =
--     Cons a $ Cons b $ interleaveStreams as bs


ruler :: Stream Integer
ruler = ruler' 0

ruler' :: Integer -> Stream Integer
ruler' n = interleaveStreams (streamRepeat n) (ruler' $ n+1)


-- Exercise 6

x :: Stream Integer
x = Cons 0 $ Cons 1 $ streamRepeat 0

scale :: Integer -> Stream Integer -> Stream Integer
scale s (Cons x xs) = Cons (s*x) $ scale s xs

instance Num (Stream Integer) where
    fromInteger n = Cons n $ streamRepeat 0
    negate (Cons x xs) = Cons (negate x) $ negate xs
    (Cons a as) + (Cons b bs) = (Cons (a + b) (as + bs))
    (Cons a as) * bs'@(Cons b bs) = Cons (a*b) $ (scale a bs) + as*bs'

instance Fractional (Stream Integer) where
    as'@(Cons a as) / bs'@(Cons b bs) =
        Cons (a `div` b) $ scale (1 `div` b) (as - (as'/bs')*bs)


fibs3 :: Stream Integer
fibs3 = x / (1 - x - x^2)


-- Exercise 7

data Matrix = Square Integer Integer Integer Integer deriving (Show, Eq)

instance Num Matrix where
    (Square x0 x1 x2 x3) * (Square y0 y1 y2 y3) =
        (Square (x0*y0+x1*y2) (x0*y1+x1*y3) (x2*y0+x3*y2) (x2*y1+x3*y3))

fib4 :: Integer -> Integer
fib4 0 = 0
fib4 n = case (Square 1 1 1 0)^n of
           (Square _ x _ _) -> x
