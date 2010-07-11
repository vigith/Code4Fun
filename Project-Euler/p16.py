# 2^(15) = 32768 and the sum of its digits is 3 + 2 + 7 + 6 + 8 = 26.

# What is the sum of the digits of the number 2^(1000)?

# LOGIC
# left shift of 1 is powering by 2, then add the digits 

(count, n) = (0, 1 << 1000)

while (n >= 1):
    count += n % 10
    n /= 10

print count
