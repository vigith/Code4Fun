# A Pythagorean triplet is a set of three natural numbers, a < b < c, for which,
# a^(2) + b^(2) = c^(2)

# For example, 3^(2) + 4^(2) = 9 + 16 = 25 = 5^(2).

# There exists exactly one Pythagorean triplet for which a + b + c = 1000.
# Find the product abc.

# LOGIC:
# If m and n are any two natural numbers,
# Let a = n2 - m2 -> (eq. 1), 
# b = 2nm -> (eq. 2), 
# c = n2 + m2 -> (eq. 3). Then, a, b, and c form a Pythagorean triple.
# and m > n
# now i have a + b + c = 1000 in question
# adding eqs. 1, 2, 3 gives me
# a + b + c = 2m^2 + 2mn
# which is equal to 1000 = 2m ^ 2 + 2mn
# now find the values of n by substituting m from 0 to 100 (100^2 = 1000, so m can't never be more than 100) :-)

import math

def solve_quad():
    a = []
    for m in range(1,100):
        n = 500.0 / m - m        
        if n < 0: break
        ## n can be integers having decimal values
        ## to avoid those see whether floor and value are same :-)
        if math.floor(n) == n:
            a.append((m,n))

    return a

def find_abc(list):
    ## If m and n are any two natural numbers,
    ## Let a = n2 - m2, b = 2nm, c = n2 + m2. Then, a, b, and c form a Pythagorean triple.
    ## and m > n
    for i in list:
        if i[0] <= i[1]:
            continue
        a = i[0] ** 2 - i[1] ** 2
        b = 2 * i[0] * i[1]
        c = i[0] ** 2 + i[1] ** 2
        
        if a ** 2 + b ** 2 == c ** 2: return a, b, c


if __name__ == '__main__' :
    m_n = solve_quad()
    ans = find_abc(m_n)
    print ans,
    print reduce(lambda x,y: x * y, ans)
