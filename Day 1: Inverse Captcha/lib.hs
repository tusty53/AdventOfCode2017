import Data.Char

same :: Int -> Int -> Int
same x y = if x == y then x else 0

countDoubleCycle :: [Int] -> Int
countDoubleCycle a = same (head a) (last a)

countDoubles :: [Int] -> Int
countDoubles (x:y:z) = same x y + countDoubles (y:z)
countDoubles x = 0

countAllDoubles :: [Int] -> Int
countAllDoubles a = countDoubles a + countDoubleCycle a

countHalfways :: Int -> Int -> [Int] -> Int
countHalfways length index xs = if length == index
    then 0
    else same (xs !! index) (xs !!  ((index + (length `div` 2)) `mod` length)) + countHalfways length (index+1) xs

textToDigits :: [Char] -> [Int]
textToDigits [] = []
textToDigits ['\n'] = []
textToDigits (x:xs) = digitToInt x : textToDigits xs




