module CollatzConjecture (collatz) where

collatz :: Integer -> Maybe Integer
collatz n = if n <= 0 then Nothing else Just (collatz' n 0)
  
collatz' :: Integer -> Integer -> Integer
collatz' 1 s = s
collatz' n s | even n    = collatz' (n `quot` 2) (s + 1)
             | otherwise = collatz' (n * 3 + 1) (s + 1)


-- Collatz Conjuncture is a graph theorem that states that 4 <- 2 <- 1 is *the only* circular graph under the function 
-- C(x) | x / 2, if x is even; and
--      | 3 * x + 1, if x is odd.
--
-- Idea: A declaritive solution could embody the graph so that a breadth first search for n, counting the longest edge would lead to step count s.
--
--       1
--       |
--       2
--       |
--       4
--       |
--       8
--       |
--      16
--     .-'----.
--    32      5
--    |
--    64        10
--  .-'---.      |---.
-- 128    21    20   3
--  |      '-.   '-. '-.
-- 256      42    40   6
--  |----.   |    /
-- 512  85  84  {13}  ...
--
-- Finding `13` in a BFS would lead to an edge length of 9 which is the number of steps it takes:
-- 13
-- -> 3*13+1 = 40  s = 1
-- -> 40 / 2 = 20  s = 2
-- -> 20 / 2 = 10  s = 3
-- -> 10 / 2 =  5  s = 4
-- -> 3*5 +1 = 16  s = 5
-- -> 16 / 2 =  8  s = 6
-- ->  8 / 2 =  4  s = 7
-- ->  4 / 2 =  2  s = 8
-- ->  2 / 2 =  1  s = 9
--
-- s0: v = 1 
-- s1: v = [2, 1]
-- s2: v = [4, 2, 1]
-- s3: v = [8, 4, 2, 1]
-- s4: v = [16, 8, 4, 2, 1]
-- s5: v = [[32, 5], 16, 8, 4, 2, 1]
-- s6: v = [[64, 10], [32, 5], 16, 8, 4, 2, 1]
-- s7: v = [[[128, 21], [20, 3]], [64, 10], [32, 5], 16, 8, 4, 2, 1]
-- s8: v = [[[256, 42], [40, 6]], [[128, 21], [20, 3]], [64, 10], [32, 5], 16, 8, 4, 2, 1]
-- s9: v = [[[[512, 85], 84], [13!, _]], [[256, 42], [40, 6]], [[128, 21], [20, 3]], [64, 10], [32, 5], 16, 8, 4, 2, 1]
-- length v-1 == 9 (we need the edges, not the nodes)


