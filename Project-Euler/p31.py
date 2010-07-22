# In England the currency is made up of pound, f, and pence, p, and there are eight coins in general circulation:

#     1p, 2p, 5p, 10p, 20p, 50p, f1 (100p) and f2 (200p).

# It is possible to make f2 in the following way:

#     1xf1 + 1x50p + 2x20p + 1x5p + 1x2p + 3x1p

# How many different ways can f2 be made using any number of coins?

# LOGIC:
#
# http://blog.dreamshire.com/docs/dyn_prog.pdf
#
# Reccurence Relation
# -------------------
# C[p] = { 
#     0, when p == 0
#     min-i : d[i] <= p {1 + C[p-d[i]]} when p > 0 
#     }
#
# At any point, if we have the minimum number of coins required to create a lower count (n - p)
# then we can think it as a subproblem for creating a larger count p


if __name__ == '__main__':
    ## table to remembering the old counts
    ways = {}
    ## for '0' value no need to select anything.. that is the only '1' way
    ways[0] = 1
    
    ## for each 'coin' in the denomintation
    for coin in (1, 2, 5, 10, 20, 50, 100, 200): ## here coin in analogous to d[i]
        ## count of each way from denomintation 1 till 200 to reach 200
        for count in range(coin, 200+1): ## this takes care of min 1 + C[p-d[i]]
            ## count is used for p
            ## current number of ways = number of ways for p - d[i] (ie smaller numbers)
            ##                           +  
            ##                          number of ways of d[i] to reach smaller numbers
            try:
                ways[count] += ways[count - coin]
            except KeyError:
                ways[count] = 1

    print ways[200]
