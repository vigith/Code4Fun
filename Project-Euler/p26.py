# A unit fraction contains 1 in the numerator. The decimal representation of the unit fractions with denominators 2 to 10 are given:

#     ^(1)/_(2)	= 	0.5
#     ^(1)/_(3)	= 	0.(3)
#     ^(1)/_(4)	= 	0.25
#     ^(1)/_(5)	= 	0.2
#     ^(1)/_(6)	= 	0.1(6)
#     ^(1)/_(7)	= 	0.(142857)
#     ^(1)/_(8)	= 	0.125
#     ^(1)/_(9)	= 	0.(1)
#     ^(1)/_(10)= 	0.1

# Where 0.1(6) means 0.166666..., and has a 1-digit recurring cycle. It can be seen that ^(1)/_(7) has a 6-digit recurring cycle.

# Find the value of d < 1000 for which ^(1)/_(d) contains the longest recurring cycle in its decimal fraction part.

# LOGIC:
# 1. The number is said to repeat if the same remainder as before appears again
# 2. powers of 2 and divisible by 5 will always non-recurring


def getRecurring(divisor):
    remainders = {}
    
    remainder = None
    ## x / y = z, x is called divident, y divisor and z quotient
    num = 1
    counter = 0

    ## remove powers of 2 and numbers divisible by 5
    if num & (num - 1): return 0
    elif (num % 5 == 0): return 0
    
    while True:
        num *= 10
        quo = num / divisor
        remainder = num % divisor
        num = remainder

        ## else will loop ever for perfect divisors
        if remainder == 0: return 0

        if not remainders.has_key(remainder): remainders[remainder] = counter
        else: 
            return counter -  remainders[remainder]

        counter += 1

    return None ## never here

if __name__ == '__main__':
    large = 0
    num   = -9
    for i in range(1, 1000):
        rec_num = getRecurring(i)
        if rec_num > large: 
            large = rec_num
            num = i

    print num, large
