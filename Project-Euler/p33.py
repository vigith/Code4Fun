# The fraction ^(49)/_(98) is a curious fraction, as an inexperienced mathematician in attempting to simplify it may incorrectly believe that ^(49)/_(98) = ^(4)/_(8), which is correct, is obtained by cancelling the 9s.

# We shall consider fractions like, ^(30)/_(50) = ^(3)/_(5), to be trivial examples.

# There are exactly four non-trivial examples of this type of fraction, less than one in value, and containing two digits in the numerator and denominator.

# If the product of these four fractions is given in its lowest common terms, find the value of the denominator.

# LOGIC:
# get the possible numbers that can divide the numerator. Since it is only two digit the return 
# of generatePossibles() will be 2 single digit numbers. 

# BETTER LOGIC:
# xy/yz = x/z
# (10x + y)/(10y+z) = x/z
# 9xz + yz = 10xy 
#  3 Equations and 3 Unknowns

def numToArray(num):
    array = []

    while(num >= 1):
        array.append(num % 10)
        num /= 10

    return array


## is 12 is the number, possible denominators are
## 11, 12, .. 19 and 11, 21, 31, 41, 51..91 for 1 and for 2
## 21, 22, 23, 24, 25, .. 29 and 22, 32, 42 .. 92
def generatePossibles(num):
    numbers = []
    for i in numToArray(num):
        ## all numbers divisible by 10 will divided by 0
        ## condition is its 10s digit is cut off
        if num == 0: continue
        numbers.extend([i * 10 + j for j in range(1, 10)])
        numbers.extend([j * 10 + i for j in range(1, 10)])

    return numbers

## take two numbers, create two list
## and do list substract
def listSubstract(num1, num2):
    num1_l = numToArray(num1)
    num2_l = numToArray(num2)

    nr = []
    dr = []
    for i in range(0, len(num1_l)):
        ## get elements for nr not in dr
        if num1_l[i] in num2_l: 
            index = num2_l.index(num1_l[i])
            num2_l[index] = None
        elif num1_l[i]: nr.append(num1_l[i])

        ## get elements for dr not in nr
        if num2_l[i] in num1_l: 
            index = num1_l.index(num2_l[i])
            num1_l[index] = None
        elif num2_l[i]: dr.append(num2_l[i])

    ## this will have only one element each (generatePossibles will create only that
    ## kind of numbers)
    try:
        return (nr[0], dr[0])
    except IndexError:
        return (1, 1)


def curiousFraction():
    dr_p = 1
    nr_p = 1
    for i in range(11, 100):
        for j in generatePossibles(i):
            if i >= j or i % 10 == 0 or j % 10 == 0: continue
            else:
                (nr, dr) = listSubstract(i, j)
                if float(nr) / dr == float(i) / j:
                    dr_p *= j
                    nr_p *= i

    ## catch: the answer looks like '00' once everything got cancelled out, but it is 100
    return dr_p, nr_p

if __name__ == '__main__':
    print curiousFraction()
