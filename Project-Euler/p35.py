# The number, 197, is called a circular prime because all rotations of the digits: 197, 971, and 719, are themselves prime.

# There are thirteen such primes below 100: 2, 3, 5, 7, 11, 13, 17, 31, 37, 71, 73, 79, and 97.

# How many circular primes are there below one million?

# LOGIC:
# Get prime numbers till 10**6, rotate and see whether each rotated one is prime (see wether there is any
# even number in the number, if that is the case rotation will provide composite numbers).
# I should write the sieve's algo, i am using an old iterator approach

import math

primes_hash = {}

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
    if num < 0: return False
    elif num == 1 or num == 2: return True
    elif primes_hash.has_key(num): return True
    
    ## even numbers
    if not num & 1:
        return False

    for x in range(3, int(num**0.5)+1, 2):
        if num % x == 0:
            return False

    ## update the hash
    primes_hash[num] = True
    return True


def numToArray(num):
    array = []
    while num >= 1:
        array.append(num % 10)
        num /= 10

    array.reverse()
    
    return array

def rotations(num_a):
    array = [num_a]

    for i in range(0, len(num_a) - 1):
        tmp = [array[-1][-1]]
        tmp.extend(array[-1][:-1])
        array.append(tmp)

    return array


def rotatePrime(num):
    num_array = numToArray(num)
    if num < 10:
        return True

    for i in (0, 2, 4, 6, 8, 5):
        if i in num_array:
            return False
    
    for array in rotations(num_array):
        num = int(''.join([str(i) for i in array]))
        if not isPrime(num):
            return False

    return True
        

if __name__ == '__main__':
    limit = 1000000
    primes = []
    it = naturalNumbers(2)
    count = 0
    while True:
        next_prime = nextPrime(primes, it)
        if next_prime > limit: break
        if (rotatePrime(next_prime)):
            count += 1
        primes.append(next_prime)

    print count
