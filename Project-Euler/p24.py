# A permutation is an ordered arrangement of objects. For example, 3124 is one possible permutation of the digits 1, 2, 3 and 4. If all of the permutations are listed numerically or alphabetically, we call it lexicographic order. The lexicographic permutations of 0, 1 and 2 are:

# 012   021   102   120   201   210

# What is the millionth lexicographic permutation of the digits 0, 1, 2, 3, 4, 5, 6, 7, 8 and 9?

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


if __name__ == '__main__':
    perm = permuteFunc(range(0,10), [])
    perm.sort()
    print perm[1000001-2]
