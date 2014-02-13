module Golf where


skips    :: [a] -> [[a]]
skips xs = map f indices
    where f n = map snd $ filter (\(i,_) -> i `mod` n == 0) $ zip indices xs
          indices = [1..(length xs)]

localMaxima :: [Integer] -> [Integer]
localMaxima ints = map g $ filter f $ zip3 ints (tail ints) $ tail $ tail ints
    where f (l,n,m) = n > l && n > m
          g (_,n,_) = n

histogram      :: [Integer] -> String
histogram ns = (f $ map (count ns) [0..9]) ++ "==========\n0123456789\n"
    where count [] _ = 0
          count (x:xs) y = (if x == y then 1 else 0) + (count xs y)
          f :: [Integer] -> String
          f freqs = g h freqs
              where g 0 _ = ""
                    g n freqs =
                        (map (\x -> if x >= n then '*' else ' ') freqs) ++ "\n" ++
                        g (n-1) freqs
                    h = maximum freqs




