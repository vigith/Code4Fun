# The decimal number, 585 = 1001001001_(2) (binary), is palindromic in both bases.

# Find the sum of all numbers, less than one million, which are palindromic in base 10 and base 2.

# (Please note that the palindromic number, in either base, may not include leading zeros.)

# LOGIC:
# No even number's base 2 equivalent will have palindromic string. Check the number, if
# it is a palindrome then check for base

# GOOD LOGIC:
# Also, for numbers under 1000000, palindromes are of one of these forms:
# a
# aa
# aba
# abba
# abcba
# abccba
# A can be either 1, 3, 5, 7, or 9, while b and c can 0, 1, 2...8, 9. So the amount of numbers in each form are:
# a - 5
# aa - 5
# aba - 50
# abba - 50
# abcba - 500
# abccba - 500
# This adds up to a total of 1110 possibilities for base 10 palindromes, instead of 999999. 

def isPalindrome(array):
    length = None
    if len(array) % 2 == 0: length = len(array) / 2
    else: length = len(array) / 2 + 1
    for i in range(length):
        if array[i] != array[-i-1]:
            return False

    return True

def toBase(num, base):
    array = []
    
    while num >= 1:
        array.append(num % base)
        num /= base

#    array.reverse()
    return array


if __name__ == '__main__':
    limit = 10**6
    count = 0
    while limit > 0:
        i = limit
        ## even numbers will have binary number ending in 0
        if i & 1 == 0: next
        if isPalindrome(toBase(i, 10)):
            if isPalindrome(toBase(i, 2)):
                count += i
        limit -= 1

    print count
