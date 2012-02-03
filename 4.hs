-- A palindromic number reads the same both ways. The largest palindrome made from the product of two 2-digit numbers is 9009 = 91 99.
-- Find the largest palindrome made from the product of two 3-digit numbers.


{-- This is inefficient, but I couldn't see how to do a list 
	comprehension which I could alternate counters like this:
	a  b
	5  5
	4  5
	4  4
	3  4
	...

	Then I wouldn't have to guarantee N^2, it would just be theoretical worst case .


UPDATE: 
It seems a much faster approach is to start from the maximum product,
find all of the palindromes through descent iteration.  Then, test each 
found palindrome for a modulus of 0 and a that both divisors are in the
acceptable range

For our example 
Highest Product : N*N => 100*100  = 10000
Palindromes (descending):
9999
9889
9779
..
9009

This means we only need to check the divisor constraints on 
10 palindromes before finding the answer

More generally. 
xyyx Results in 9 < xy < 100, or  
(N-N/10) palindromes to check in the worst case


--}

{--
Starts at the largest product in the range
Recursively finds descending palindromes
Checks for divisor conditions, first found wins
--}
getLargestPalindrome n = findNextPalindrome (n*n)
	where
		findNextPalindrome 1 = 1
		findNextPalindrome x 
			| isPalindrome(show x) && threeDigitFactors x = x
			| otherwise =  findNextPalindrome (x-1) 


{--
Checks the divisor conditions
For some d in (99,1000), the following must hold
where the the palindrome being checked is x

x `mod` d == 0
x `div` d in (99,1000)
 
--}
threeDigitFactors x  = modAndRangeCheck x 999
	where 
		modAndRangeCheck x 99 = False
		modAndRangeCheck x d 
			| x `mod` d == 0 = 
				let r = x `div` d 
				in if(r > 99 && r < 1000) then True
					else modAndRangeCheck x (d-1)
			| otherwise = modAndRangeCheck x (d-1)
	


-- Check if the string and its reverse are the same
isPalindrome s = s == reverse s

-- Find the largest which is returned
problem4 n = getLargestPalindrome n


main = do print (problem4 1000)
