toViableInt :: Double -> Integer
toViableInt a = if x `mod` 2 == 1 then x else x+1
                where x = ceiling a

find_sqrt :: Integer -> Integer
find_sqrt = toViableInt . sqrt . fromIntegral

find_first_move :: Integer -> Integer
find_first_move a = floor ((fromIntegral a)/2)

find_right_pos_number :: Integer -> Integer -> Integer
find_right_pos_number my_sqrt step= (my_sqrt - 2)^2 + step

find_dist_to_number :: Integer -> Integer -> Integer
find_dist_to_number a b = abs (a-b)

find_final_distance :: Integer -> Integer
find_final_distance n = step + find_closest_mid n my_sqrt step
    where   my_sqrt = find_sqrt n
            step = find_first_move my_sqrt

find_closest_mid :: Integer -> Integer -> Integer -> Integer
find_closest_mid n my_sqrt step = minimum ( map (\x -> find_dist_to_number (right_pos +  x * step) n ) [0,2,4,6])
    where right_pos = find_right_pos_number my_sqrt step