# You are given the following information, but you may prefer to do some research for yourself.

#     * 1 Jan 1900 was a Monday.
#     * Thirty days has September,
#       April, June and November.
#       All the rest have thirty-one,
#       Saving February alone,
#       Which has twenty-eight, rain or shine.
#       And on leap years, twenty-nine.
#     * A leap year occurs on any year evenly divisible by 4, but not on a century unless it is divisible by 400.

# How many Sundays fell on the first of the month during the twentieth century (1 Jan 1901 to 31 Dec 2000)?


# LOGIC:
# Find out the start day for each Year. Now given the start day and the year, we can find the start day of each month

week_days = {0 : 'Sun', 1 : 'Mon', 2 : 'Tue', 3 : 'Wed', 4 : 'Thu', 5 : 'Fri', 6 : 'Sat'}

def leapYear(year):
    if (year % 4 == 0 and year % 100 != 0) or year % 400 == 0:
        return True
    else: return False

def daysInYear(year):
    if leapYear(year):
        return 366

    return 365

def daysInMonth(year):
    days = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    if leapYear(year):
        days[1] = 29
        return days

    return days

def sunsOnFirst(start_day, year):
    days = daysInMonth(year)
    no_of_fst_suns = 0

    for day in range(len(days)):
        if start_day == 0: 
            no_of_fst_suns += 1
        end_day = days[day] % 7     
        start_day = (end_day + start_day) % 7


    return no_of_fst_suns


## No use of this function, I read the question wrongly
def noOfSuns(start_day):
    no_suns = 0
    ## I know there are 31 days in Jan
    no_of_weeks = 31 / 7
    no_suns += no_of_weeks
    ## 31 % 7 = 3
    ## so if it starts on Sunday, then +2 including Sunday will include Monday
    if start_day == 0 or ((start_day + 2) % 7 < start_day and (start_day + 2) % 7 >= 0):
        no_suns += 1

    return no_suns

if __name__ == '__main__':
    ## 8th day will be the same day
    ## we know it started on Monday, so next start day of the year will be 1 + next
    suns_on_first = 0
    start_day = 1
    for year in range(1901, 2001):
        ## - 1 because we are finding the start date and start date depends on the nomber of days in 
        ## prev year
        end_day = daysInYear(year - 1) % 7     
        start_day = (end_day + start_day) % 7
        suns_on_first_this_yr = sunsOnFirst(start_day, year)
        suns_on_first += suns_on_first_this_yr
        print week_days[start_day], "is the start day of", year, "and has", suns_on_first_this_yr, "Sundays on 1st"

    print "Number of Suns of 1st", suns_on_first
        
        
