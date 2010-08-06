# The number 3797 has an interesting property. Being prime itself, it is possible to continuously remove digits from left to right, and remain prime at each stage: 3797, 797, 97, and 7. Similarly we can work from right to left: 3797, 379, 37, and 3.

# Find the sum of the only eleven primes that are both truncatable from left to right and right to left.

# NOTE: 2, 3, 5, and 7 are not considered to be truncatable primes.

import math

# LOGIC:
# Straight Logic, using 1 parse we can get both the left and right shifted numbers. I can avoid rechecking whether a number is prime
# twice using an array (but lazy to do that)

# BETTER LOGIC:
# A truncable prime generated will be formed (loosely used) from the similar already generated truncable prime
# 23
# 37
# 53
# 73
# 313
# 317
# 373
# 797
# 3137
# 3797
# 739397

## can add an optimization by *memoizing* factors
def nextPrime(list, it):
    while True:
        next_num = it.next() 
        flag = 0
        sqrt = next_num ** .5
        for i in list:
            if next_num % i == 0:
                flag = 1
                break
            if sqrt < i:
                flag = 0
                break
        if flag: continue
        else: return next_num


def naturalNumbers(init):
    counter = init

    while True:
        yield counter
        counter += 1


def isPrime(num):
    if num <= 1: return False
    elif num == 2: return True
    
    ## even numbers
    if not num & 1:
        return False

    for x in range(3, int(num**0.5)+1, 2):
        if num % x == 0:
            return False

    return True        

def isTruncPrime(num):
    if not isPrime(num):
        return False
    string = str(num)
    if string[-1] in [2, 4, 6, 8, 9]: return False
    if string[0] in [4, 6, 8, 9]: return False

    ## rotate
    rev = 10
    o_num = num
    array1 = []
    array2 = []
    while num >= 1:
        if not isPrime(num):
            return False
        elif not isPrime(o_num % rev):
            return False
        array1.append(num)
        array2.append(o_num % rev)
        rev *= 10

        num /= 10

    print array1, array2
    return array1, array2

if __name__ == '__main__':
    limit = 10000
    count = 0
    it = naturalNumbers(8)
    primes = [2, 3, 5, 7]
    ssum = 0
    while count < 11:
        np = nextPrime(primes, it)
        primes.append(np)
        if isTruncPrime(np):
            print np
            ssum += np
            count += 1

    print ssum


