{--
The following iterative sequence is defined for the set of positive integers:

n  n/2 (n is even)
n  3n + 1 (n is odd)

Using the rule above and starting with 13, we generate the following sequence:

13  40  20  10  5  16  8  4  2  1
It can be seen that this sequence (starting at 13 and finishing at 1) contains 10 terms. Although it has not been proved yet (Collatz Problem), it is thought that all starting numbers finish at 1.

Which starting number, under one million, produces the longest chain?

NOTE: Once the chain starts the terms are allowed to go above one million.
--}

{--
NOTE: This is a fairly slow implementation.  I describe in the comments below what I suspect to be the causes of the inefficiency.
--}

import Data.List
import Data.Array
{--
This basically sets up an immutable array to store previous calculated values.
It seems like a bum memoization.  
This populates the array with tuples to record the index and the length of 
the chain
--}
mkArray f bounds = array bounds [(i, f i 0) | i <- range bounds]

{--
For all elements under 1mill I pass their values in to the creation 
of the memo array via the f function.  

So f is called while the array is being calculated
--}
followArray = mkArray f (0,1000000) where
    f 1 c = 1
    f n c = if ((n `mod` 2 == 0)) then (follow (n `div` 2) (c+1))+1
		  else  (follow ((3*n )+1) (c+1))+1

{--
This takes into account all of the 	values of f(x) where 
the values exceeds the bounds of the immutable array.  This
is the SLOW zone.  

There should be a way to do this with an infinite list.  I had some
problems getting multiple arguments into the fibonacci example for 
memoization.
--}
hardFollow n c = if ((n `mod` 2 == 0)) then (follow (n `div` 2) (c+1))+1
		  else  (follow ((3*n )+1) (c+1))+1


{--
Decide whether to use the memo method or the slow method
--}
follow x c =  if(x > 1000000) then hardFollow x c
			  else followArray  ! x 

{--
Just find the maximum tuple (f(x),x) and get the second
number which is the start for the longest chain

Use a list comprehension to force the population of all
f(x)
--}
problem14 n =  maximumBy (compare) [(follow x 0,x) | x <- [1..n]]


main = do print $ problem14 1000000
