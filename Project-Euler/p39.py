# If p is the perimeter of a right angle triangle with integral length sides, {a,b,c}, there are exactly three solutions for p = 120.

# {20,48,52}, {24,45,51}, {30,40,50}

# For which value of p <= 1000, is the number of solutions maximised?

# LOGIC:
# 1. A + B > C
# 2. A^2 + B^2 = C^2
# 3. A + B + C = n and 4 < n < 1000
# 4. C > A and C > B
# 5. A <= n / 2 and B <= n / 2 and C <= n / 2
# 6. lets tabe A < (always) B , else B and A may get reversed
# 7. given A and B, C can be found since i know n.
# Given, a^2+b^2 = c^2 (1)
# 8. If both a and b are even, c will also be even and P (the perimeter) will be even.
# 9. If both a and b are odd, c will be even and P will be even.
# 10. If one is even and the other is odd, c will be odd and P will again be even.
# 11. Therefore, only even values of P need to be checked. 

if __name__ == '__main__':
    count = 4
    max = [-1, -1]
    while count <= 1000:
        hit = 0
        for i in range(1, count / 2):
            ## avoid reversal
            if i * 2 > count / 2: break
            for j in range(i, count / 2):
                ## cond 7
                k = count - (i + j)
                ## cond 1
                if k < i + j: next
                ## cond 2
                if (i ** 2 + j ** 2 == k ** 2):
                    hit += 1
        if max[0] < hit:
            max = [hit, count]

        count += 2

    print max
