import Data.List.Split
import Data.List

minmaxSum :: [Int] -> Int
minmaxSum [] = 0
minmaxSum x = maximum x - minimum x

checkEvenDiff :: Int -> [Int] -> Int
checkEvenDiff n [] = 0
checkEvenDiff n (x:xs) = if (n `mod` x) == 0 then n `div` x else checkEvenDiff n xs

evenDiv :: [Int] -> Int
evenDiv [] = 0
evenDiv [x] = 0
evenDiv (a:b:xs) = if x /= 0 then x else evenDiv (b:xs)
                        where x = checkEvenDiff a (b:xs)

sortAndDiv :: [Int] -> Int
sortAndDiv x = evenDiv (reverse(sort x))

localChecksumStrings :: ([Int] -> Int) -> [String] -> Int
localChecksumStrings f x = f (map (read::String->Int) x)

localChecksumString :: ([Int] -> Int) -> String -> Int
localChecksumString f x = localChecksumStrings f (splitOn "\t" x)

checkSum :: ([Int] -> Int) -> [String] -> Int
checkSum f [] = 0
checkSum f [x] = localChecksumString f x
checkSum f (x:xs) = localChecksumString  f x + checkSum f xs



