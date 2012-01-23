-- 2520 is the smallest number that can be divided by each of the numbers from 1 to 10 without any remainder.
-- What is the smallest positive number that is evenly divisible by all of the numbers from 1 to 20?

import Debug.Trace

findEvenMultiple divisors multiple i
	| all (\x -> (multiple*i) `mod` x == 0) divisors = (multiple*i) -- Check if all divisors divide evenly into current multiple
	| otherwise =  findEvenMultiple divisors multiple (i+1)			-- Increase the multiple

-- Start at the LCM of n, n-1
problem5 n =  findEvenMultiple [1..n] (n*(n-1)) 1

main = do print ((problem5 20))
