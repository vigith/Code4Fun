# The following iterative sequence is defined for the set of positive integers:

# n -> n/2 (n is even)
# n -> 3n + 1 (n is odd)

# Using the rule above and starting with 13, we generate the following sequence:
# 13 -> 40 -> 20 -> 10 -> 5 -> 16 -> 8 -> 4 -> 2 -> 1

# It can be seen that this sequence (starting at 13 and finishing at 1) contains 10 terms. Although it has not been proved yet (Collatz Problem), it is thought that all starting numbers finish at 1.

# Which starting number, under one million, produces the longest chain?

# NOTE: Once the chain starts the terms are allowed to go above one million.

# LOGIC:
# Find the chain, remember the elements for which it has calculated once
# use the remembered value if while solving the chain.
# Also we know that, even numebrs are 1 greater than their odd counterparts ie chain of 3 + 1 is answer for 6

cache = {}

class LessThanOne:
    pass


## this is recursive method, could be slow
## stack may get blown-off
def applyRule(num, chain):
    orig_num = chain[0]
    if cache.has_key(num):
        cache[orig_num] = cache[num] + len(chain) - 1 ## last element is the first element of cached array
        return cache[orig_num]
    
    if num == 1:
        cache[orig_num] = len(chain)
        return cache[orig_num]
    elif num < 1:
        raise LessThanOne

    if num % 2 == 0:
        rule = num / 2
        chain.append(rule)
        return applyRule(rule, chain)
    else:
        rule = 3 * num + 1
        chain.append(rule)
        return applyRule(rule, chain)

## iterative method of above the recursive method
def applyRuleIt(num, chain):
    orig_num = chain[0]

    while num >= 1:
        if cache.has_key(num):
            cache[orig_num] = cache[num] + len(chain) - 1 ## last element is the first element of cached array
            return cache[orig_num]
    
        if num == 1:
            cache[orig_num] = len(chain)
            return cache[orig_num]
        elif num < 1:
            raise LessThanOne

        if num % 2 == 0:
            num /= 2
            chain.append(num)
        else:
            num = 3 * num + 1
            chain.append(num)


    cache[orig_num] = len(chain)        
    return cache[orig_num]


if __name__ == '__main__':
    counter = 1
    largest = [None, None]

    while counter < 1000000:
        result = applyRuleIt(counter, [counter,])
        if counter & 1:
            cache[counter * 2] = result + 1
        if result > largest[1]:
            largest[1] = result
            largest[0] = counter

        counter += 1


    print largest

            
