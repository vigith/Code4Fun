# Surprisingly there are only three numbers that can be written as the sum of fourth powers of their digits:

#     1634 = 1^(4) + 6^(4) + 3^(4) + 4^(4)
#     8208 = 8^(4) + 2^(4) + 0^(4) + 8^(4)
#     9474 = 9^(4) + 4^(4) + 7^(4) + 4^(4)

# As 1 = 1^(4) is not a sum it is not included.

# The sum of these numbers is 1634 + 8208 + 9474 = 19316.

# Find the sum of all the numbers that can be written as the sum of fifth powers of their digits.


# LOGIC:
# Adding 5 digit number can produce 6 digits. So max value will be 9^5*6 = 354294. Iterate
# from 2 to 354294 and see whose digits' power-to-sum = number


def numToArray(num):
    res = []
    while num >= 1:
        res.append(num % 10)
        num /= 10

    res.reverse()
    return res


def sumOfPow(array, pow):
    return reduce(lambda x,y: x + y, [ i ** pow for i in array])

if __name__ == '__main__':
    max = 9 ** 5 * 6
    total = 0
    for i in range(2, max):
        if sumOfPow(numToArray(i), 5) == i:
            total += i


    print total
