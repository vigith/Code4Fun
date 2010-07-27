# 145 is a curious number, as 1! + 4! + 5! = 1 + 24 + 120 = 145.

# Find the sum of all numbers which are equal to the sum of the factorial of their digits.

# Note: as 1! = 1 and 2! = 2 are not sums they are not included.

# LOGIC:
# Two types of implementation is in here
# 1. sum(1! + 2! .. 9!) = 409113, that means it has only 6 digits so take 9C6 from range(0, 10)
#   1.a. do sum of factorial and see wehther it is equal to ''.join.(digits)
# 2. brute force, from 1 to 409113 (it could be more than that)
#   2.a see whether sum of factorials = num


import math

def numEqToFacts(array):
    string = str(sum([math.factorial(i) for i in array]))
    s_a = [ int(i) for i in string ]
    s_a.sort()
    array.sort()
    if s_a == array:
        return True
    # length = len(string)

    # ## x is not EXACTLY equal to xx
    # if length != len(array):
    #     return False

    

    # ## is x = x?
    # for i in range(0, length):
    #     if int(string[i]) != array[i]:
    #         return False

    return False

def numToArray(num):
    array = []
    while num >= 1:
        array.append(num % 10)
        num /= 10

    array.reverse()
    
    return array


def arrayToNum(array):
    return int(''.join([str(i) for i in array]))


def get_combination(list, com):
    array = []
    if len(com) == 0:
        array.extend([[i] for i in list[:]])
    else:
        for i in com:
            index = list.index(i[-1])
            ## can't merge anything with a fully grown array
            for j in list[index+1:]:
                k = i[:]
                k.append(j)
                array.append(k) 
    return array

def combination(r, list, com):
    if r < 1:
        return com
    
    return combination(r - 1, list, get_combination(list, com))

## combination with repetition
def get_combination_r(list, com):
    array = []
    if len(com) == 0:
        array.extend([[i] for i in list[:]])
    else:
        for i in com:
            index = list.index(i[-1])
            ## can't merge anything with a fully grown array
            for j in list[index:]:
                k = i[:]
                k.append(j)
                array.append(k) 
    return array

## combination with repetition
def combination_r(r, list, com):
    if r < 1:
        return com
    
    return combination_r(r - 1, list, get_combination_r(list, com))


def test():
    print "here"
    print combination(1, [1,2,3,4], [])
    print combination(2, [1,2,3,4], [])
    print combination(3, [1,2,3,4], [])
    print combination(4, [1,2,3,4], [])

        

if __name__ == '__main__':
    limit = 10
    for i in range(3, limit):
        array = combination_r(len(str(math.factorial(i))), range(0, limit), [])
        for j in array:
            if (numEqToFacts(j)):
                print j
                
    # total_sum = 0
    # for i in xrange(3, 46233):
    #     break
    #     if (numEqToFacts(numToArray(i))):
    #         total_sum += i

    # print total_sum
