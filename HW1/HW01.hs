{-# OPTIONS_GHC -Wall #-}
module HW01 where

-- Exercise 1 -----------------------------------------

-- Get the last digit from a number
lastDigit :: Integer -> Integer
lastDigit n = n `mod` 10

-- Drop the last digit from a number
dropLastDigit :: Integer -> Integer
dropLastDigit n = n `div` 10

-- Exercise 2 -----------------------------------------

toRevDigits :: Integer -> [Integer]
toRevDigits n
    | n <= 0    = []
    | otherwise = (lastDigit n) : (toRevDigits $ dropLastDigit n)

-- Exercise 3 -----------------------------------------

-- Double every second number in a list starting on the left.
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther []       = []
doubleEveryOther (x:[])   = [x]
doubleEveryOther (x:y:zs) = x : (2*y) : (doubleEveryOther zs)

-- Exercise 4 -----------------------------------------

-- Calculate the sum of all the digits in every Integer.
sumDigits :: [Integer] -> Integer
sumDigits []     = 0
sumDigits (x:xs) = (sumDigitsHelper x) + (sumDigits xs)

-- Calculate the sum of all the digits in an Integer.
sumDigitsHelper :: Integer -> Integer
sumDigitsHelper n
    | n < 0     = (-1) * (sumDigitsHelper (-n))
    | n == 0    = 0
    | otherwise = (lastDigit n) + (sumDigitsHelper $ dropLastDigit n)


-- Exercise 5 -----------------------------------------

-- Validate a credit card number using the above functions.
luhn :: Integer -> Bool
luhn n = ((sumDigits $ doubleEveryOther $ toRevDigits n) `mod` 10) == 0

-- Exercise 6 -----------------------------------------

-- Towers of Hanoi for three pegs
type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi n start end storage
    | n == 1 = [(start, end)]
    | otherwise = (hanoi (n-1) start storage end) ++ [(start, end)] ++ (hanoi (n-1) storage end start)
