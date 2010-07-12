# n! means n x (n - 1) x ... x 3 x 2 x 1

# Find the sum of the digits in the number 100!

# LOGIC:
# get factorial using math.factorial (No Mood To Write That Code or To Think About Something Better :-) ) and add

import math

def numToArray(n):
    array = []
    while (n >= 1):
        array.append(n % 10)
        n /= 10

    array.reverse()
    return array

print reduce(lambda x, y: x + y, numToArray(math.factorial(100)))
