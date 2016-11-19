-- Q1
is_bit :: Int -> Bool
is_bit n 
        | n == 1 = True
        | n == 0 = True
        | otherwise = False

-- Q2
flip_bit :: Int -> Int
flip_bit n
         | is_bit n == False = error "n is NOT a bit"
         | n == 1 = 0
         | n == 0 = 1

-- Q3
-- (a)
is_bit_seq1 :: [Int] -> Bool
is_bit_seq1 seq
               | null seq = True
               | is_bit (head seq) == True = is_bit_seq1 (tail seq)
               | otherwise = False

-- (b)
is_bit_seq2 :: [Int] -> Bool
is_bit_seq2 seq = if null seq then True else if is_bit (head seq) == True then is_bit_seq2 (tail seq) else False

-- (c)
is_bit_seq3 :: [Int] -> Bool
is_bit_seq3 = all (is_bit)

-- Q4
invert_bits1 :: [Int] -> [Int]
invert_bits1 (x, xs) = (flip_bit x) ++ (invert_bits1 xs)







