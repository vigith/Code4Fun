# Let d(n) be defined as the sum of proper divisors of n (numbers less than n which divide evenly into n).
# If d(a) = b and d(b) = a, where a = b, then a and b are an amicable pair and each of a and b are called amicable numbers.

# For example, the proper divisors of 220 are 1, 2, 4, 5, 10, 11, 20, 22, 44, 55 and 110; therefore d(220) = 284. The proper divisors of 284 are 1, 2, 4, 71 and 142; so d(284) = 220.

# Evaluate the sum of all the amicable numbers under 10000.

# LOGIC:
# for all the numbers from 1 to 10000 find the proper divisors and look up whether the sum of proper divisors is 
# there in the number with that numbers sum of divisors equal to current number
# eg.
# hash[220] = 284 and
# hash[282] = 220
# hash will have all the numbers and the sum of divisors, but amicable_pairs will have only the the reqd pairs

import math

def properDivisors(num):
    divisors = [1]

    upper = math.ceil(num ** .5) + 1
    for i in range(2, upper):
        if num % i == 0:
            divisors.extend((i, num / i))

    return divisors


if __name__ == '__main__':
    divisors = {}
    amicable_pairs = []
    for num in range(2, 10000):
        p_divisors = properDivisors(num)
        sum_of_divisors = reduce(lambda x, y: x + y, p_divisors)
        if divisors.has_key(sum_of_divisors):
            if divisors[sum_of_divisors] == num: amicable_pairs.append((sum_of_divisors, num))
        else: divisors[num] = sum_of_divisors

    print amicable_pairs
    print reduce(lambda x, y: x + y, [ i[0] + i[1] for i in amicable_pairs])
