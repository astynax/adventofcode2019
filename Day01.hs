main :: IO ()
main = interact $ show . solve . parse . lines

parse :: [String] -> [Integer]
parse = map read

solve :: [Integer] -> Integer
solve = sum . map toFuel
  where
    -- toFuel = subtract 2 . (`div` 3)
    toFuel x = calc x - x
    calc x
      | next < 0  = x
      | otherwise = x + calc next
      where
        next = div x 3 - 2
