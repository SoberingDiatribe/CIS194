{-# OPTIONS_GHC -Wall #-}
module HW04 where

newtype Poly a = P [a]

-- Exercise 1 -----------------------------------------

x :: Num a => Poly a
x = P [0, 1]

-- Exercise 2 ----------------------------------------

instance (Num a, Eq a) => Eq (Poly a) where
    (==) (P []) (P j)          = (sum j == 0)
    (==) (P i) (P [])          = (sum i == 0)
    (==) (P (i:is)) (P (j:js))
        | i == j    = ((P is) == (P js))
        | otherwise = False

-- Exercise 3 -----------------------------------------

instance (Num a, Eq a, Show a) => Show (Poly a) where
    show (P i) = concatString $ toStr $ showExp i

showExp :: Num a => [a] -> [(a, Int)]            
showExp is = [(j, k) | (j, k) <- zip is [0..]]

toStr :: (Num a, Eq a, Show a) => [(a, Int)] -> [String]
toStr [] = []
toStr [(0, 0)] = ["0"]
toStr ((0, 0):j:js) = toStr (j:js)
toStr ((j, 0):js) = (show j) : (toStr js)
toStr ((0, _j):js) = toStr js
toStr ((j, 1):js) = (show j ++ "x") : (toStr js)
toStr ((j, k):js) = (show j ++ "x^" ++ show k) : (toStr js)

concatString :: [String] -> String
concatString []       = "0"
concatString (j:k:ks) = (concatString (k:ks)) ++ (" + " ++ j)
concatString [j] = j

-- Exercise 4 -----------------------------------------

plus :: Num a => Poly a -> Poly a -> Poly a
plus (P i) (P j) = P [kk + ll | (kk, ll) <- (zip k l)]
    where
        (k, l) = regularize i j

regularize :: Num a => [a] -> [a] -> ([a], [a])
regularize i j = case iLen `compare` jLen of
                    LT -> (extend i (jLen - iLen), j)
                    EQ -> (i, j)
                    GT -> (i, extend j (iLen - jLen))
    where
        iLen = length i
        jLen = length j

extend :: Num a => [a] -> Int -> [a]
extend [] 0     = []
extend [] i     = 0 : (extend [] (i-1))
extend (j:js) i = j : (extend js i)

-- Exercise 5 -----------------------------------------

times :: Num a => Poly a -> Poly a -> Poly a
times (P i) (P j) = sum (timesHelper i j)
    where
        timesHelper [] _ls = []
        timesHelper (k:ks) ls = (P (map (k*) ls)) : (timesHelper ks (0:ls))

-- Exercise 6 -----------------------------------------

instance Num a => Num (Poly a) where
    (+) = plus
    (*) = times
    
    negate (P i) = (P (map ((-1)*) i))
    
    fromInteger i = P [fromInteger i]
    
    -- No meaningful definitions exist
    abs    = undefined
    signum = undefined

-- Exercise 7 -----------------------------------------

applyP :: Num a => Poly a -> a -> a
applyP (P i) j = sum [c*j^e | (c, e) <- (zip i [0..])]

-- Exercise 8 -----------------------------------------

class Num a => Differentiable a where
    deriv  :: a -> a
    nderiv :: Int -> a -> a
    nderiv 0 a = a
    nderiv i a = nderiv (i-1) (deriv a)

-- Exercise 9 -----------------------------------------

instance (Num a, Enum a) => Differentiable (Poly a) where
    deriv = derivation

derivation :: (Num a, Enum a) => Poly a -> Poly a
derivation (P (i:j:js)) = P [c*e | (c, e) <- (zip (j:js) [1..])]
derivation _i = (P [])