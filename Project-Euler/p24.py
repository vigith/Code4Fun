# A permutation is an ordered arrangement of objects. For example, 3124 is one possible permutation of the digits 1, 2, 3 and 4. If all of the permutations are listed numerically or alphabetically, we call it lexicographic order. The lexicographic permutations of 0, 1 and 2 are:

# 012   021   102   120   201   210

# What is the millionth lexicographic permutation of the digits 0, 1, 2, 3, 4, 5, 6, 7, 8 and 9?

# LOGIC:
# Slower Method:
# It enumerates all the permutations and takes out the 1 millionth numbers.
# Permutation Logic:
# 1. suppose we have numbers (0,1,2) to permute and we have a permuted list with no elements
#    in beginning []
# 2. Begining from last index of the list and permuted list, if
#    there are no elements in permuted list append the last element
# 3. Else, to each element in the permuted list, apply the last index of the list
#    ie, we have (0,1) and [2]
#    do [1,2] and [2,1].. ie, insert the last element into all the positions around 2
#    no we have (0) and [[1,2] and [2,1]] in permuted list
#    here, insert 0 to 0th, 1st and 2nd postn of each element in permuted list
#    so it will become [0,1,2], [1,0,2], [1,2,0] when 0 is applied to [1,2]

import math

def appendNum(list, num):
    new = []
    for i in range(0, len(list) + 1):
        tmp = list[:]
        tmp.insert(i, num)
        new.append(tmp)

    return new

def permuteFunc(data, permute):
    if not data:
        return permute

    num = data.pop()
    ## first time
    if not permute: 
        permute.append([num])
        return permuteFunc(data, permute)

    new = []
    for i in range(len(permute)):
        perm = appendNum(permute.pop(), num)
        new.extend(perm)            


    return permuteFunc(data, new)

def slowerWay():
    perm = permuteFunc(range(0,10), [])
    perm.sort()
    return perm[1000001-2]

def fasterWay():
    """
    LOGIC:
    Find the possbile index and counter where factorial is just less than the index.
    interate till difference between least index and required index is 0 -> (1)
    """
    perm  = range(0,10)
    index = 1000000 - 1
    ans = []
    while True:
        ret = findMaxFactorial(index, len(perm))
        (counter, num) = ret
        mul  = math.factorial(num) * counter
        diff = index - mul
        if diff == 0: ## (1)
            ## pop out the num for which diff == 0
            odd = perm.pop(-num-1)
            ## take all the elements in perm list (w/o the num for which diff == 0)
            ## and give it to answer list
            ans.extend(perm)
            ## put the pop'ed out number back to the ans list
            ans.append(odd)
            return ans
        else:
            ## The num returned by findMaxFactorial is the set of remaining elements in perm list
            ## on which permutation should be applied and the numbers without this
            ## index can be copied directly to the ans list.
            ## Now from the rest of the elements in the perm list, pop out the counter element

            ## the elements before the num
            odd = perm[:-counter - num]
            ## put the list to answer list
            ans.extend(odd)
            ## take out the remaining elements to pull out the counter'th element
            perm = perm[-counter - num:]
            ## append the counter'th element
            ans.append(perm.pop(counter))
            index = diff

    

## find the Max factorial less than
## the given index with given numbers
def findMaxFactorial(index, num):
    """
    LOGIC:
    First try num !, if it is greater than index then find (num - 1)! and go on till we find 
    the one below the limit. This need not be the Max below the limit, so do
    proper multiple of (num - x) ! till we get the least.

    RETURN:
    the counter and the numbers on which factorial can be applied, counter by default is 1 (because
    we index from back in the callee
    """
    fact = math.factorial(num)
    if (fact > index):
        return findMaxFactorial(index, num - 1)
    elif (fact < index):        
        counter = 1
        ## suppose the index > 9! and < 10!
        ## it means from 9 elements, find out the pivot such that the permutation of it is 
        ## closest to the index
        while True:
            mul = fact * counter
            if mul < index:
                counter += 1
            elif mul >= index:
                return (counter - 1, num)
    else:
        return (1, num)
                

if __name__ == '__main__':
    # print slowerWay()
    print fasterWay()
