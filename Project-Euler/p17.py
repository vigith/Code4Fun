# f the numbers 1 to 5 are written out in words: one, two, three, four, five, then there are 3 + 3 + 5 + 4 + 4 = 19 letters used in total.

# If all the numbers from 1 to 1000 (one thousand) inclusive were written out in words, how many letters would be used?

# NOTE: Do not count spaces or hyphens. For example, 342 (three hundred and forty-two) contains 23 letters and 115 (one hundred and fifteen) contains 20 letters. The use of "and" when writing out numbers is in compliance with British usage.

convention = {
    0 : 'zero',
    1 : 'one',
    2 : 'two',
    3 : 'three',
    4 : 'four',
    5 : 'five',
    6 : 'six',
    7 : 'seven',
    8 : 'eight',
    9 : 'nine',
    10 :'ten',
    11 : 'eleven',
    12 : 'twelve',
    13 : 'thirteen',
    14 : 'fourteen',
    15 : 'fifteen',
    16 : 'sixteen',
    17 : 'seventeen',
    18 : 'eighteen',
    19 : 'nineteen',
    20 : 'twenty',
    30 : 'thirty',
    40 : 'forty',
    50 : 'fifty',
    60 : 'sixty',
    70 : 'seventy',
    80 : 'eighty',
    90 : 'ninety',
    'H' : 'hundred',
    'T' : 'thousand',
    }

def numToArray(n):
    array = []
    while (n >= 1):
        array.append(n % 10)
        n /= 10

    array.reverse()
    return array

def mapToWords(array, num):
    count = 0
    zero_flag = 0
    words = []
    while array:
        top = array.pop()
        if count == 0:
            words.append(convention[top])
        elif count == 1:
            if top == 0 and not zero_flag:
                ## don't add zero if the number
                ## before it was too zero
                if num % 10 != 0: 
                    zero_flag = 1
                    words.append('and')
                else: pass
            else:
                if num % 100 >= 10 and num % 100 <= 20: 
                    ## remove the top element, since it got joined
                    words.pop()
                    words.append(convention[num % 100])
                else: 
                    zero_flag= 1
                    words.append(convention[top*10])
                    if num > 100: words.append('and')
        elif count == 2:
            if top == 0 and not zero_flag:
                zero_flag = 1
                if num % 100 != 0: 
                    zero_flag = 1
                    words.append('and')
                else: pass
            else:
                words.append(convention[top] + ' ' + convention['H'])
        elif count == 3:
            words.append(convention[top] + ' ' + convention['T'])
        else:
            pass
        
        count += 1

    words = [i for i in words if i != 'zero']

    if not zero_flag and len(words) == 2:
        words.insert(1, 'and')

    words.reverse()
    return words

def mapToWordsNoSpace(array, num):
    count = 0
    zero_flag = 0
    words = []
    while array:
        top = array.pop()
        if count == 0:
            words.append(convention[top])
        elif count == 1:
            if top == 0 and not zero_flag:
                ## don't add zero if the number
                ## before it was too zero
                if num % 10 != 0: 
                    zero_flag = 1
                    words.append('and')
                else: pass
            else:
                if num % 100 >= 10 and num % 100 <= 20: 
                    ## remove the top element, since it got joined
                    words.pop()
                    words.append(convention[num % 100])
                else: 
                    zero_flag= 1
                    words.append(convention[top*10])
                    if num > 100: words.append('and')
        elif count == 2:
            if top == 0 and not zero_flag:
                zero_flag = 1
                if num % 100 != 0: 
                    zero_flag = 1
                    words.append('and')
                else: pass
            else:
                words.append(convention[top] + '' + convention['H'])
        elif count == 3:
            words.append(convention[top] + '' + convention['T'])
        else:
            pass
        
        count += 1

    words = [i for i in words if i != 'zero']

    if not zero_flag and len(words) == 2:
        words.insert(1, 'and')

    words.reverse()
    return words

def test():
    print mapToWords([9,8], 98)
    print mapToWords([2,1,1,1], 2111)
    print mapToWords([2,1,0,1], 2101)
    print mapToWords([2,0,1,1], 2011)
    print mapToWords([1,1], 11)
    print mapToWords([1,0,0], 100)
    print mapToWords([2,0,0,0], 2000)

if __name__ == '__main__':
#    test()
    count = 0
    for i in range(1,1001):
        count += reduce(lambda x,y : x + y, [ (len(i)) for i in mapToWordsNoSpace(numToArray(i), i)])

    print count
