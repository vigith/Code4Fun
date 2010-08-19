# Take the number 192 and multiply it by each of 1, 2, and 3:

#     192 x 1 = 192
#     192 x 2 = 384
#     192 x 3 = 576

# By concatenating each product we get the 1 to 9 pandigital, 192384576. We will call 192384576 the concatenated product of 192 and (1,2,3)

# The same can be achieved by starting with 9 and multiplying by 1, 2, 3, 4, and 5, giving the pandigital, 918273645, which is the concatenated product of 9 and (1,2,3,4,5).

# What is the largest 1 to 9 pandigital 9-digit number that can be formed as the concatenated product of an integer with (1,2, ... , n) where n > 1?

# LOGIC:
# Brute force with better boundaries.
# Cases:
# 1. Multiplicand starts with 1 and is sequential
# 2. Total length of concatenated products == 9
# Iterate till we have cat(prod) > 9 and store if isPandigital

def isPandigital(num):
    """
    use count sort to arrange since i know the input set and if there is a 'hole'
    or if there is a 'hit' (same number repeats) then fail
    """
    
    count_sort = [0,0,0,0,0,0,0,0,0,0]
    count = 0
    while(num >= 1):
        rem = num % 10
        
        ## pandigital from 1 to 9 (not 0)
        if rem == 0:
            return False
        
        try:
            ## is it same number again?
            if count_sort[rem] == 0: count_sort[rem] = rem
            else: return False
        except:
            return False
        
        num /= 10
        count += 1

    ## did i over shoot?
    if count == 9: return True
    
    return False

if __name__ == '__main__':
    count = 2
    pand  = []
    while True:
        flag = -9
        prod = ''
        for i in range(1,10):
            res  = str(i * count)
            flag = i

            ## attach to prod only if len of prod < 9
            if (len(prod) + len(res) > 9):
                break
            else:
                prod += res

        if (isPandigital(int(prod))):
            print prod, i, count
            pand.append(prod)
        ## questions says n > 1
        if flag == 2:
            break

        count += 1

    print max(pand)
            
