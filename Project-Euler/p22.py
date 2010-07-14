# Using names.txt (right click and 'Save Link/Target As...'), a 46K text file containing over five-thousand first names, begin by sorting it into alphabetical order. Then working out the alphabetical value for each name, multiply this value by its alphabetical position in the list to obtain a name score.

# For example, when the list is sorted into alphabetical order, COLIN, which is worth 3 + 15 + 12 + 9 + 14 = 53, is the 938th name in the list. So, COLIN would obtain a score of 938 x 53 = 49714.

# What is the total of all the name scores in the file?

# LOGIC:
# Get the names, strip off the sort and strip off the ". then find the ord(char) - ord('A') + 1

file = "data/names.txt"
names = []

fd = open(file, 'r')
for line in fd.readlines():
    names.extend(line.split(','))
fd.close()

## remove the " in name
names = [ name[1:-1] for name in names]
names.sort(cmp)

counter = 1
total_score = 0
upper = ord('A') - 1
for name in names:
    value = reduce(lambda x, y: x + y, [ ord(i) - upper for i in name])
    score = value * counter
    total_score += score
    counter += 1

print total_score
