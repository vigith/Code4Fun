# By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, we can see that the 6^(th) prime is 13.

# What is the 10001^(st) prime number?


# LOGIC
# To find all the prime numbers less than or equal to a given integer n by MODIFIED (by me :-) 'Sieve_of_Eratosthenes' method:

#    1. Create a list of consecutive integers from two to n: (2, 3, 4, ..., n).
#    2. Initially, let p equal 2, the first prime number.
#    3. Strike from the list all multiples of p less than or equal to n. (2p, 3p, 4p, etc.)
#    4. Find the first number remaining on the list after p (this number is the next prime); replace p with this number.
#    5. Repeat steps 3 and 4 until p2 is greater than n.
#    6. All the remaining numbers in the list are prime.

# a number is prime if it is not divisbile by any of the prime numbers which occured before
# start from 2 in a list and get the next number from natural numbers, try to divide it by the elements in the list
# if none from the list is able to divide then it is prime and if we can ADD an extra condn that, "if primes in list till SQRT of num didn't divide"
# then too it is a prime (it avoids checking lot many numbers, for 101, i need to check only till 11 (2,3,5,7,11) :-) )


def nextNum():
    counter = 2
    while True:
        yield counter
        counter += 1 

## requires the whole of list to be created, so will take more time
def nextPrimeSlow(list, it):
    while True:
        next_num = it.next() 
        if not [ i for i in list if next_num % i == 0 ]: return next_num

def nextPrime(list, it):
    while True:
        next_num = it.next() 
        flag = 0
        sqrt = next_num ** .5
        for i in list:
            if next_num % i == 0:
                flag = 1
                break
            if sqrt < i:
                flag = 0
                break
        if flag: continue
        else: return next_num

if __name__ == "__main__":
    from time import time
    start = time()
    primes = [2,]
    count = 1

    nth_prime = 10001
    it = nextNum()
    while (count < nth_prime):
        next_prime = nextPrime(primes, it)
        primes.append(next_prime)
        count += 1

    print "Ans for Nth Prime (", nth_prime, ") is", primes[-1], ", and it took ", time()-start, " seconds"
