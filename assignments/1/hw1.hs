-- | Return the digits of the input integer.
--   For 0 or negative integer, return an empty list.
toDigits :: Integer -> [Integer]
toDigits n = (reverse . toDigitsRev) n

-- | Do the same as toDigits, except the
--   digits are in reverse order.
toDigitsRev   :: Integer -> [Integer]
toDigitsRev n
    | n == 0 = []
    | n < 0 = []
    | otherwise = (mod n 10) : (toDigitsRev $ div n 10)

-- | Double every other digits, starting from the last one.
doubleEveryOther    :: [Integer] -> [Integer]
doubleEveryOther ns = map (\(i,n) -> if p i then 2*n else n) (zip [0..] ns)
    where p = if odd $ length ns then odd else even

-- | Sum all digits
sumDigits :: [Integer] -> Integer
sumDigits = sum . concat . map toDigits


-- | Validate whether the input integer could be a valid credit card number
validate   :: Integer -> Bool
validate n = (sumDigits . doubleEveryOther . toDigits $ n) `mod` 10 == 0


-- | The Tower of Hanoi
type Peg = String
type Move = (Peg, Peg)


hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi n from to temp =
    if n == 1
    then [(from,to)]
    else (hanoi (n-1) from temp to) ++
             [(from,to)] ++ (hanoi (n-1) temp to from)

hanoi4                       :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi4 n from to temp1 temp2 =
    if n == 1 then [(from,to)]
    else
        let k = n `div` 2 in
        (hanoi k from temp1 to)
        ++ (hanoi (n-k) from to temp2)
               ++ (hanoi k temp1 to from)

