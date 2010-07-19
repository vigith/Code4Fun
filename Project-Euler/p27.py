# Euler published the remarkable quadratic formula:

# n^2 + n + 41

# It turns out that the formula will produce 40 primes for the consecutive values n = 0 to 39. However, when n = 40, 40^(2) + 40 + 41 = 40(40 + 1) + 41 is divisible by 41, and certainly when n = 41, 41^2 + 41 + 41 is clearly divisible by 41.

# Using computers, the incredible formula  n^2 - 79n + 1601 was discovered, which produces 80 primes for the consecutive values n = 0 to 79. The product of the coefficients, -79 and 1601, is -126479.

# Considering quadratics of the form:

#     n^2 + an + b, where |a| < 1000 and |b| < 1000

#     where |n| is the modulus/absolute value of n
#     e.g. |11| = 11 and |-4| = 4

# Find the product of the coefficients, a and b, for the quadratic expression that produces the maximum number of primes for consecutive values of n, starting with n = 0.

# LOGIC:
# 1) b must be positive prime; (since we start with n = 0, b should be prime)
# 2) a must be odd; 
#   if a in even
#     odd + even +prime = odd + prime = even (not prime)
#   if a in odd
#     odd + odd +prime = even + prime = odd (can be prime)
# 3) when n = 1 (Ques says we start from 0) AND we know b is +ve prime (logic 1)
#    1 + a + b = 0
#    so, a < -1 -b or
#        b > -1 -a


primes_hash = {}

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

def primesInQuad(a, b):
    counter = 0
    primes = []
    while True:
        eqn = counter ** 2 + a * counter + b
        if isPrime(eqn): primes.append(counter)
        else: break
        counter += 1

    return len(primes)
        

if __name__ == '__main__':
    LOWER = -999
    UPPER = 1000
    (max_i, max_j, primes) = (None, None, 0)
    for a in range(LOWER, UPPER, 2): ## indexing by 2 will make it iterate through odd numbers (logic 2)
        for b in range(-1-a, UPPER): # logic 3
            # logic 1
            if not isPrime(b): continue
            no_of_primes = primesInQuad(a,b)
            if no_of_primes > primes:
                max_i = a
                max_j = b
                primes = no_of_primes

    print max_i * max_j
