# A perfect number is a number for which the sum of its proper divisors is exactly equal to the number. For example, the sum of the proper divisors of 28 would be 1 + 2 + 4 + 7 + 14 = 28, which means that 28 is a perfect number.

# A number n is called deficient if the sum of its proper divisors is less than n and it is called abundant if this sum exceeds n.

# As 12 is the smallest abundant number, 1 + 2 + 3 + 4 + 6 = 16, the smallest number that can be written as the sum of two abundant numbers is 24. By mathematical analysis, it can be shown that all integers greater than 28123 can be written as the sum of two abundant numbers. However, this upper limit cannot be reduced any further by analysis even though it is known that the greatest number that cannot be expressed as the sum of two abundant numbers is less than this limit.

# Find the sum of all the positive integers which cannot be written as the sum of two abundant numbers.

# LOGIC:
# 1. it is not an abundant number if it is not divisbile by 2 or 5
# 2. after 20162 all are abuntant numbers
# 3. all perfect multiple of an abuntant number is an abuntant number
# 4. abuntant number is sum of divisors (remove duplicates, eg. 16 is not an abuntant number)

import math

abundant = {}

## remove duplicates, 
## eg, 16 has 1,16, 2,8, 4,4 (don't include 4) (LOGIC 4)
def sumOfDivisors(num):
    ## check in hash
    if abundant.has_key(num): return True

    divisors = {1:1}

    upper = int(num ** .5) + 1
    for i in range(2, upper):
        if num % i == 0:
            divisors[i] = 1
            divisors[num / i] = 1

    return reduce(lambda x, y: x + y, divisors.keys())


def isAbundantNumber(num):
    ## not even and divisible by 5 (LOGIC 1)
    if not (num % 2 == 0 or num % 5 == 0): return False

    if sumOfDivisors(num) > num:
        return True
    else: return False


## LOGIC 3
def derivedAbundantNums(num, upper):
    upper_l = int(upper/num) + 1
    for i in range(1, upper_l):
        mul = num * i
        if mul > upper: break
        if abundant.has_key(mul):
            pass
        else:
            abundant[mul] = 1


if __name__ == '__main__':
    LIMIT = 20162
    for i in range(12, LIMIT):
        if isAbundantNumber(i):
            derivedAbundantNums(i, LIMIT)

    tmp = {}
    a_sort = abundant.keys()
    a_sort.sort()
    length = len(a_sort)
    ## get the sum of all the elements in the abundant array
    for i in range(length):
         ## not range(i+1, length) because a number can be added to itself
        for j in range(i, length):
            sum = a_sort[i] + a_sort[j]
            if sum >= LIMIT: break
            else: tmp[sum] = 1

    total = 0
    ## filter the array (remove the sum of abundant numbers)
    for i in range(LIMIT):
        if tmp.has_key(i): pass
        else: total += i

    print total
