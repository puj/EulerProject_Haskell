{--
Starting in the top left corner of a 2x2 grid, there are 6 routes (without backtracking) to the bottom right corner.


How many routes are there through a 20x20 grid?
--}
import Data.Array
import Data.Functor
import Control.Applicative


{--
This counts total routes to each cell up to nx,ny

We start by creating an array which we will store all of the routes.

For each element in the array, we sum the routes from the North and Western neighbours
If one coordinate is 0, the number of routes is 1
If one corrdinate is 1, the number of routes is the other coordinate plus 1

6 = 3 + 3
3 = 1 + 2, etc.
111
123
136

So f (2,2) = f(1,2) + f(2,1)
--}
countRoutes nx ny = r
    where r = listArray (0, ((nx+1)*(ny+1)-1)) $  map f $ flip (,) <$> [0..nx] <*> [0..ny]
          f x 
          	| fst(x) <= 1 || snd(x) <= 1 = fst(x)*snd(x)+1
          	| otherwise =   (r!(((snd(x)-1)* (ny+1) +fst(x))))+
          					(r!(((snd(x))* (ny+1) +fst(x)-1)))

{--
Get the last route calculated
--}
problem15 nx ny = ( countRoutes nx ny) ! ((nx+1)*(ny+1)-1)

main = do print $ problem15 20 20
