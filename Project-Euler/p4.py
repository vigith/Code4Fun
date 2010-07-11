# A palindromic number reads the same both ways. The largest palindrome made from the product of two 2-digit numbers is 9009 = 91 x 99.

# Find the largest palindrome made from the product of two 3-digit numbers.


## LOGIC:
## start from a = 999 and b = 999, start multiplying. since it grows smaller break when the largest we got is
## less than the multiple (x * y and y * x is done, we can live with this :-)

def multiply():
    largest = 1;
    a = 999
    while a > 100:
        b = 999
        while b > 100:
            m = a * b

            ## we are coming from bigger to smaller
            if m < largest:
                break

            if is_palindrome(m) and m > largest:
                largest = m

            b -= 1

        a -= 1

    print largest


def is_palindrome(m):
    n = m
    r = 0
    # rev 123 = 321
    while n > 0:
        r = 10*r + n % 10
        n /= 10
        
    if r == m:
        return True

    return False


multiply()
