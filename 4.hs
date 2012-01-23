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
--}

-- Get all possible products which are palindrome
	-- I don't know how to do the list comprehension more succintly... reverse hack..
getProducts n = [(i*j) | i <- reverse[1..n] , j <- reverse[1..n], isPalindrome(show  (i*j)) ]

-- Check if the string and its reverse are the same
isPalindrome s = s == reverse s

-- Find the largest which is returned
problem4 n = maximum (getProducts n)


main = do print ((problem4 1000))
