# The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.

# Find the sum of all the primes below two million.

## i could have used sieve_of_eratosthenes algo as it is, but lazy :-) .. this is not that bad
## gives ans in 13 secs

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

    max_prime = 2000000
    it = nextNum()
    while (max_prime > primes[-1]):
        next_prime = nextPrime(primes, it)
        primes.append(next_prime)

    primes.pop()
    print "Ans for Sum till Max Prime (", max_prime, ") is", reduce(lambda x,y: x + y, primes), ", and it took ", time()-start, " seconds"
