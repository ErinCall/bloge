Title: Annotation Station: hanke.py
DisqusId: 3
Slug: annotation-station-hanke-py
Posted: 2012-01-17T06:59:00Z
Tags:
    python
    annotation station
A week or so ago, I annotated a chunk of code for a co-worker who was complaining that open-source did him little good if he didn't understand the language. I was surprised--and pleased--to find that I learned a lot from the exercise myself. I resolved immediately to do it some more. This is the first in what will be several trillion annotated chunks of code.

This is a python script written to convert between Gregorian (you may know them as 'normal') dates and dates in the [Hanke-Henry calendar](http://henry.pha.jhu.edu/calendar.html). It is written by github user [elsewherean](https://github.com/elsewherean).

The Hanke-Henry calendar is a bad idea. It isn't clear what problems it solves, but the many problems inherent in switching to a whole new calendar system are evident. However, we can still have fun and learn something from analyzing elsewherean's code. Let's get to it, shall we?

``` Python
# import a couple of functions for manipulating standard gregorian dates
from datetime import date,timedelta

#this'll define a collection of functions that can all operate on Hanke calendars.
class PermanentCalendar:
    #the __init__ function is called when initializing a python object.
    #self will implicitly be bound to a new object. The other params are optional;
    #they default to the given values.
    def __init__(self,year=2012,month=1,day=1):
        #do some object setup. the `special` and `start` attributes are set on
        #the object `self`
        #`special` is initialized as an empty dict.
        #I wonder what it's for! Nothing ever uses it.
        self.special = {}
        #`start` is initialized as an immutable tuple containing (presumably) a
        #year, month, and day.
        self.start = (2012,1,1)
        #note that the year/month/day params passed in above were not used.
        #they now pass out of scope, never to be seen again. Hm.
    #this function will, presumably, convert the given date from a gregorian
    #date to a Hanke date.
    def convertFromGregorianDate(self,year,month,day):
        #calculate the timedelta between the given gregorian date and the
        #baseline date, 20120101, then report what Hanke date is that many days
        #after the baseline date.

        #note the use of subtraction on a pair of datetime.date objects.
        #Python will call the __sub__ function on the left-hand date with the
        #right-hand date as its argument.
        diff = date(2012,1,1)-date(year,month,day)
        #since Hanke-Henry takes effect 20120101, one can assume any date
        #desired in Hanke-Henry is after that. So it's a little odd that the
        #previous line subtracted the desired (larger) date from the baseline
        #(smaller) date. Below, the timedelta is inverted, to give a positive
        #argument to daysFrom.
        return self.daysFrom(diff.days*-1)
    #this'll convert a Hanke date to a gregorian date.
    def convertToGregorianDate(self,year,month,day):
        #create a `diff` counter. In a minute, this'll represent the timedelta
        #in days between the given date and the baseline date.
        diff = 0
        #create a tuple holding the given gregorian date.
        origday = (year,month,day)
        #create a tuple holding the baseline date.
        day = (2012,1,1)
        #loop until the baseline date has been adjusted to match the given
        #date. On each loop, increment or decrement the baseline date by one
        #day to bring it closer to the given day. Track how many loops have
        #happened; that will be the difference in days between the given date
        #and the baseline date.

        #this is a fairly standard "days since epoch" approach to date math,
        #and it's a classic for a reason. The author avoids the temptation to
        #try to move in chunks--decrement the year counter and add 365, for
        #example--which, while tempting, is likely to run into heck of edge
        #cases.
        while (day!=origday):
            if year < 2012:
                day = self.decrDay(day)
                diff -=1
            else:
                day = self.incrDay(day)
                diff +=1
        #now delegate to the timedelta and date objects provided by python's
        #datetime module.
        newday = date(2012,1,1)+timedelta(diff)
        #finally, the datetime object is split into the year-month-day tuple
        #system that's used throughout PermanentCalendar.
        return (newday.year,newday.month,newday.day)
    #this function will tell us whether the given year has an extra week
    #in the Hanke system.
    def has_extra_week(self,year):
        #date.weekday will return the day of week, counting from Monday = 0,
        #of the given date. I don't see a difference between
        #`date.weekday(date(year,1,1))` and `date(year,1,1).weekday()`. The
        #author has seemed mistrustful in general of OOP (and given the
        #atrocities that've been committed in its name, who can blame him?).
        #In any case, a year has an extra week if its gregorian counterpart
        #starts or ends on a Thursday. Hmm.
        return date.weekday(date(year,1,1)) == 3 or date.weekday(date(year,12,31))==3

    #Given a date, return the date that is one day later.
    #Note the unusual function signature: this function takes a year/month/day
    #tuple, but unpacks it, as though it had simply been passed a year, month,
    #and day individually.
    def incrDay(self,(year,month,day)):
        #Ah, date math. It will become rapidly evident to the reader that
        #whatever else the Hanke calendar accomplishes, it doesn't cut down on
        #edge cases.
        #First edge case: last day of the year.
        if month == 12 and day == 31:
            #if this year has an extra week, it's not the last day of the year
            #after all. It's the last day of the month.
            if self.has_extra_week(year):
            #Return the first day of next month.
                return (year,month+1,1)
            else:
            #Return the first day of next year.
                return (year+1,1,1)
        #Next edge case: REALLY the last day of the year, for seriously.
        elif month == 13 and day ==7:
            #return the first day of next year.
            return (year+1,1,1)
        #Every third month in the Hanke calendar has 30 days. If the modulus-3
        #of the month is 0--that is, if the month is a multiple of 3--the last
        #day of the month is the 31st.
        elif month % 3 == 0 and day == 31:
            #Return the first day of next month.
            return (year,month+1,1)
        #For other months, the last day is the 30th.
        elif month % 3 > 0 and day == 30:
            #return the first day of next month.
            return (year, month+1,1)
        #That's the end of the edge cases.
        else:
            #just return the next day of this month.
            return (year, month, day+1)
    #Return the date that is one day before the given date.
    #This function has the same interesting tuple-in-function-signature pattern
    #that incrDay had, above.
    def decrDay(self,(year,month,day)):
        #And we're right back into edge cases. If it's the first day of the
        #month, the day before will be the previous month...
        if day == 1:
            #which might have been in the previous year...
            if month == 1:
                #which might have had an Xtr week...
                if self.has_extra_week(year-1):
                    #return the last day of last year's Xtr week.
                    return (year-1,13,7)
                else:
                    #return the last day of last year.
                    return (year-1,12,31)
            #last month was in this year, but we still need to figure out how
            #many days it had. As we saw above, every third month has 31 days.
            #Check to see if last month was one of those:
            elif (month-1) % 3 == 0:
                #return the last day of the previous month.
                return (year,month-1,31)
            #if it's not, it's a 30-day month.
            else:
                #return the last day of the previous month.
                return (year, month-1,30)
        #we're finally out of the "first day of the month" case.
        else:
            #return the previous day of this month.
            return (year,month,day-1)
    #this function returns the Hanke date that is the given number of days after
    #the given Hanke date (or the baseline date, if no date was provided).
    def daysFrom(self,count,day=None):
        #if `day` wasn't provided, default it to the baseline date.
        if day is None:
        #note that this code references self.start for the baseline date,
        #rather than constructing it on the fly.
            day = self.start
        #if the given count is negative, find a date before the baseline date,
        #rather than after.
        if count < 0:
            #range() will create a list of numbers starting at the first
            #argument and ending at the second argument, *noninclusive*,
            #stepping equal to the third argument each time. So if `count` is
            #-5, for example, it will return a list like [0, -1, -2, -3, -4].
            #Then we'll loop through that list, assigning each element to `y`
            #and executing the code in the loop.
            #Note that y is not actually accessed. The loop doesn't care about
            #the contents of the list, it just wants to be executed `count`
            #times.
            for y in range(0,count,-1):
                #step down by one day.
                day = self.decrDay(day)
        else:
            #Just as above, step forward one day, `count` many times.
            for y in range(0,count,1):
                day = self.incrDay(day)
        return day

#this concludes the definition of the PermanentCalendar class.
#from this point on the code will execute as soon as the script runs, rather
#than waiting to be called as a function.

#create a PermanentCalendar object. Note that this calls the __init__ function
#defined way back at the beginning.
x = PermanentCalendar()
#Whoa, python golf! There is *so much* happening on this line. I'm gonna
#annotate below instead of above, so the reader can get a look at it before
#I start talking.
#In short, though, it prints today's date in the Hanke calendar.
print x.convertFromGregorianDate(*map(lambda x:int(x),date.today().isoformat().split('-')))
#the first thing that happens is actually right in the middle:
#  date.today().
#  this generates a new date object for today's date.
#next:
#  .isoformat()
#  isoformat() returns a string in a standardized format. At the time of
#  writing, date.today().isoformat() looks like '2012-01-16'.
#next:
#  .split('-')
#  split is a function on strings that breaks them up into lists. It looks
#  through the string, closing it off and starting a new one every time it
#  encounters the given token. In this case, we're splitting on '-', so the
#  string we got back from isoformat() will break up into year, month, and day.
#  At this point they are still strings, not integers.

#So that's the second argument to the `map` function. The first one is a
#lambda, an anonymous function defined on-the-fly. This one takes a single
#argument `x` and passes it along to the `int` function.
#Now the lambda and the year-month-day list are passed to the `map` function.
#`map` will apply its first argument to each element of the list in its second
#argument. In this case, it'll return a list containing the year, month, and
#day--much as we got back from `split`, except now they're integers instead of
#strings.

#but wait, what's that * doing in there? Well, in a method call like this, it
#will expand the elements of the list that came out of `map`, so we'll be
#passing the year, month, and day to the convertFromGregorianDate method,
#rather than passing *a list containing them*. This is an important
#distinction, no matter what Larry Wall thinks.

#Finally, the `print` statement sends the Hanke-formatted date to the terminal.
```

Oof, that last line is pretty hard to understand, huh. What do you say we re-write it?

First of all, there's no need to define that lambda at all. We can use `int` directly:

``` Python
print x.convertFromGregorianDate(*map(int,date.today().isoformat().split('-')))
```

Except we don't really need the map at all. It turns out that `isoformat().split('-')` is a really odd way to get at the date's members, since date objects can tell you about the year/month/date directly.

``` Python
today = date.today()
print x.convertFromGregorianDate(today.year, today.month, today.day)
```

Ok, I can live with it now.
