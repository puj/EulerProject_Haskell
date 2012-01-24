{--
A Pythagorean triplet is a set of three natural numbers, a < b < c, for which,

a^2 + b^2 = c^2
For example, 3^2 + 4^2 = 9 + 16 = 25 = 5^2.

There exists exactly one Pythagorean triplet for which a + b + c = 1000.
Find the product abc.
--}

{-- This has N^2 time because we set z to the difference of n (1000) and (x+y).  
This assures the condition a+b+c == n
--}
problem9 n =  [x*y*z | x <- [1 .. n-2] , y <- [x+1 .. n-1] , let z = n-(x+y) ,  x**2+y**2 == z**2]

main = do print ( (problem9 1000))