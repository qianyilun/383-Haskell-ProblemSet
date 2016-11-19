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
-- (a)
invert_bits1 :: [Int] -> [Int]
invert_bits1 seq 
                | length seq == 1 = [flip_bit (head seq)]
                | otherwise = (++) [(flip_bit (head seq))] (invert_bits1 (tail seq))

-- (b)
invert_bits2 :: [Int] -> [Int]
invert_bits2 seq = map flip_bit seq

-- (c)
-- invert_bits3 :: [Int] -> [Int]
-- invert_bits3 seq | length seq == 1 = [flip_bit (head seq)]
--                  | otherwise = [x] ++ xs
--                    where x = flip_bit (head seq)
--                          xs = invert_bits3 [n | n <- (tail seq), True]



-- invert_bits4 :: [Int] -> [Int]
-- invert_bits4 seq = take (length seq) [n | n <- (++) [(flip_bit (head seq))] 
--                                                      (invert_bits4 (tail seq)), True]

invert_bits3 :: [Int] -> [Int]
invert_bits3 seq = [flip_bit bit | bit <- seq, True]



