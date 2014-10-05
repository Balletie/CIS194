toDigits :: Integer -> [Integer]
toDigits x
  | x <= 0    = []
  | otherwise = (toDigits (x `div` 10)) ++ lastDigit x : []

toDigitsRev :: Integer -> [Integer]
toDigitsRev x = reverse . toDigits $ x

lastDigit :: Integer -> Integer
lastDigit x = x `mod` 10

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther (x:xs)
  | xs == []                 = [x]
  | (length xs) `mod` 2 == 0 = x : doubleEveryOther xs
  | otherwise                = x * 2 : doubleEveryOther xs

sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x:xs)
  | xs == []    = x
  | x < 10      = x + sumDigits xs
  | x == 10     = 1 + sumDigits xs
  | otherwise   = x `div` 10 + x `mod` 10 + sumDigits xs

validate :: Integer -> Bool
validate x = (sumDigits . doubleEveryOther . toDigits) `mod` 10  == 0
