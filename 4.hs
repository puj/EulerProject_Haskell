-- A palindromic number reads the same both ways. The largest palindrome made from the product of two 2-digit numbers is 9009 = 91 99.
-- Find the largest palindrome made from the product of two 3-digit numbers.


getProducts n = [(i*j) | i <- reverse[1..n] , j <- reverse[1..n], isPalindrome(show  (i*j)) ]

isPalindrome s = s == reverse s

problem4 n = maximum (getProducts n)


main = do print ((problem4 1000))
