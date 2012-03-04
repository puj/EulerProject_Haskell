{--
2^15 = 32768 and the sum of its digits is 3 + 2 + 7 + 6 + 8 = 26.

What is the sum of the digits of the number 2^1000?
--}
--(round (total/(10**x)) ) `mod` 10 

import Control.Monad
import Control.Monad.ST

import Data.Array.IArray
import Data.Array.ST
import Data.STRef

{-- 
Calculates an UArray of the digits in reverse order of 
2^n.  Len is the expected number of digits
--}

calculate n len = 
		runSTUArray $
      	do 
      		-- Initialize the array
	      	a <- newArray  (0, len) 0 :: ST s (STUArray s Int Int)

	      	-- And the first element to 2^0
	      	writeArray a 0 1

      		-- For each power of 2
      		forM [0..(n-1)] (\pow -> do

      			-- Multiply all digits by 2
      			forM [0..(len-1)] (\j -> do
      				-- Get value at j into curr
	    			curr <- readArray a j	
	    			curr' <- newSTRef (curr * 2)	
	    			curr <- readSTRef curr'

	    			-- Save value in current index
	    			writeArray a j curr
      				)


	      		-- Carry the overflow all the way out
	      		forM [0..(len-1)] (\j -> do
	      			-- Get current value
	      			curr <- readArray a j	

	    			-- Get  value at j + 1 into next
	    			next <- readArray a (j+1) 

	    			-- Calculate how much to carry
	    			carry' <- newSTRef (curr `div` 10)
	    			carry <- readSTRef carry'

	    			
	    			if(curr >= 10) then do
	    				-- Carry the remainder to next element in array
    					writeArray a (j+1) (carry + next)

    					-- Make sure this element is under 10
    					writeArray a j (curr `mod` 10) 
    				else 
    					-- Dummy write... bad.
    					writeArray a j $ curr
	    			)
    			)
	    		
	      	return a


{--
Runs the calculate method, precalculates the number of digits in the answer
and returns as a list
--}
getCalculationAsList n = 
	let len = (floor(logBase 10 (2**n))) +1 
		in let uarray = (calculate n len)
			in [uarray ! x | x <- [0..(len-1)]]

{--
Reverse the list containin the answer and return the sum
--}
reverseAndSum n = 
	 sum ( reverse  (getCalculationAsList n ))

problem16 n = reverseAndSum n 

main = do print $ problem16 1000

