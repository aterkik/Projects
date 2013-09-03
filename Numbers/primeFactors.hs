import Data.List (delete)
import System.IO

main = do
       putStr "Please enter a positive integer: "
       hFlush stdout
       input <- getLine
       let n = (read input)::Integer
       putStrLn ("Prime factors for "
                  ++ show n ++ " are: " ++ show (allPrimes n))

factor :: Integer -> Integer -> Bool
factor x y = mod x y == 0

prime :: Integer -> Bool
prime x | x >= 2 = not (any (factor x) (candidates x))
        | otherwise = False
       where candidates x = delete x [2, 3 ..
                                     ceiling (fromInteger x ** 0.5) + 1]

primeFactors :: Integer -> [Integer]
primeFactors x = foldr step [] possibleFactors
                 where step y acc | factor x y && prime y = y : acc
                                  | otherwise = acc
                       possibleFactors = delete x [2, 3 .. 
                                                  truncate
                                                    (fromInteger x ** 0.5)
                                                      + 1] ++ [x]

getExponents :: Integer -> [Integer] -> [Integer]
getExponents n (x:xs) | mod n x == 0 = x : (getExponents (quot n x) (x:xs))
                      | prime n = [n]
                      | otherwise = getExponents n xs
getExponents _ [] = []

allPrimes :: Integer -> [Integer]
allPrimes n = getExponents n (primeFactors n)
