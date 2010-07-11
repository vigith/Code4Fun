# Starting in the top left corner of a 2 x 2 grid, there are 6 routes (without backtracking) to the bottom right corner.

# How many routes are there through a 20 x 20 grid?

# LOGIC
# There are 2 ways for each box (East, South) or (South, East). For 20 grids there will be 2 x 20 E or S strings
# Now, we know that there will be equal E and S in the path string to get to solution.
# now the question is how many different strings of size 2n can be made with equal E and S (ie, nE and nS)
# Ans is 2n C n

import math

n = 20

print math.factorial(2 * n)/ (math.factorial(20) * math.factorial(20))
