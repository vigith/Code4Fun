#!/usr/bin/env python

# Question:
# Minimum number of coins required for making a change for n amount. Given d denomination of coins.
# eg. even 1$, 2$ and 5$ coin we can make 8$ by 5$ + 2$ + 1$

d = [1, 2, 5]  # denominations
k = 3          # number of denominatios (len(d))
n = 20         # amount for with change is required

c = []   ## count of coins required for each index (amount)
s = []   ## coin index for each amount

## initiallize both to 0
for t in range(0, n+1):
    c.append(0)
    s.append(0)
    
for p in range(1, n+1):
    min = 99999
    for i in range(0, k):  ## this iterates between 0 to k
        if d[i] <= p:
            if 1 + c[p-d[i]] < min:
                min  = 1 + c[p-d[i]]
                coin = i
    c[p] = min
    s[p] = coin

print c
print s
