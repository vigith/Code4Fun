# The Fibonacci sequence is defined by the recurrence relation:

#     F_(n) = F_(n-1) + F_(n-2), where F_(1) = 1 and F_(2) = 1.

# Hence the first 12 terms will be:

#     F_(1) = 1
#     F_(2) = 1
#     F_(3) = 2
#     F_(4) = 3
#     F_(5) = 5
#     F_(6) = 8
#     F_(7) = 13
#     F_(8) = 21
#     F_(9) = 34
#     F_(10) = 55
#     F_(11) = 89
#     F_(12) = 144

# The 12th term, F_(12), is the first term to contain three digits.

# What is the first term in the Fibonacci sequence to contain 1000 digits?

# LOGIC:
# Direct application of the reccurence relation

def fibonacci(num, prev1, prev2):
    if num == 1 or num == 0:
        return (1, prev1)
    else: return (prev1 + prev2, prev1)

if __name__ == '__main__':
    counter = 3
    limit = 10 ** 1000 ## this has 1001 digits
    limit /= 10
    prev1 = 1
    prev2 = 1
    while True:
        (prev1, prev2) = fibonacci(counter, prev1, prev2)
        if  prev1 - limit >= 0:
            print counter
            break
        counter += 1
