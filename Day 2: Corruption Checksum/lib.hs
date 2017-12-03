import Data.List.Split

localChecksumInts :: [Int] -> Int
localChecksumInts [] = 0
localChecksumInts x = maximum x - minimum x

localChecksumStrings :: [String] -> Int
localChecksumStrings x = localChecksumInts (map (read::String->Int) x)

localChecksumString :: String -> Int
localChecksumString x = localChecksumStrings (splitOn "\t" x)

checkSum :: l[String] -> Int
checkSum [] = 0
checkSum [x] = localChecksumString x
checkSum (x:xs) = localChecksumString x + checkSum xs



