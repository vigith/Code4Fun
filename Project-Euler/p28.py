# Starting with the number 1 and moving to the right in a clockwise direction a 5 by 5 spiral is formed as follows:

# 21 22 23 24 25
# 20  7  8  9 10
# 19  6  1  2 11
# 18  5  4  3 12
# 17 16 15 14 13

# It can be verified that the sum of the numbers on the diagonals is 101.

# What is the sum of the numbers on the diagonals in a 1001 by 1001 spiral formed in the same way?

# LOGIC:
# Find the equation for each diagonal element, and interate till the given number (for better performance,
# we can add the temporary result and keep it in an var, unlike here where i keep the result in an array and
# add later, O(n) extra)

# BETTER LOGIC: (more mathematical)
# n is odd, if n == even then formulae will change
# corners are given by: n2, n2-n+1, n2-2n+2, and n2-3n+3, so adding this gives 4n2-6n+6.
# create a loop from 3 to 1001 in steps of 2 and find the running total (starting from 1) of the quadratic.

def findDiagonalsOfSqr(num):
    diagonals = []
    ## from num to 1, skiping every alternate element
    for i in range(num, 0, -2):
        sqr  = i ** 2
        dia1 = sqr
        ## at intersection it is 1,1 so only one 1 should be taken
        if dia1 != 1: dia2 = dia1 - i + 1
        else: dia2 = 0
        diagonals.append((dia1, dia2))

    ## from 2 to num - 1, skiping every alternate element
    for i in range(2, num, 2):
        sqr  = i ** 2
        dia1 = sqr + 1
        dia2 = dia1 - i
        diagonals.append((dia1, dia2))

    return diagonals


if __name__ == '__main__':
    UPPER = 1001
    print reduce(lambda x,y: x + y, [i[0] + i[1] for i in findDiagonalsOfSqr(UPPER)])
