# We shall say that an n-digit number is pandigital if it makes use of all the digits 1 to n exactly once; for example, the 5-digit number, 15234, is 1 through 5 pandigital.

# The product 7254 is unusual, as the identity, 39 x 186 = 7254, containing multiplicand, multiplier, and product is 1 through 9 pandigital.

# Find the sum of all products whose multiplicand/multiplier/product identity can be written as a 1 through 9 pandigital.
# HINT: Some products can be obtained in more than one way so be sure to only include it once in your sum.


# LOGIC:
# produc should not be greater than 4 digits because the count will go more than 9
# otherwise. Limit is 9876 to 9876 ** .5

def isPandigital(num):
    """
    use count sort to arrange since i know the input set and if there is a 'hole'
    or if there is a 'hit' then fail
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

def getPandigitalProduct():
    pandigital = {}
    ## product should not be greater digits then 4 digits
    ## (because the count will go more than 9)
    for i in xrange(2, 9876):
        for j in xrange(2, 100): ## len(9876 * 100) > 5
            mult = i * j
            if len(str(i) + str(j) + str(mult)) > 10:
                break
            num = int(str(i) + str(j) + str(mult))
            if (isPandigital(num)):
                pandigital[mult] = 1

    return pandigital.keys()


if __name__ == '__main__':
    products = getPandigitalProduct()
    print reduce(lambda x,y: x+y, products)

