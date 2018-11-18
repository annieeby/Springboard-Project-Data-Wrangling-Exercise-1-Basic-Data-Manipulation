library(dplyr)
library(tidyr)
library(lubridate)

f <- file.choose()
d <- read.csv(f)
d
#Explore the data.
class(d)
dim(d)
names(d)
glimpse(d)
summary(d)
sum(is.na(d))

#Complete cases tell us which rows have no missing values.
complete.cases(d)

#Omit any rows with missing values would be a fast and dirty solution here. (Not doing.)
#na.omit(d)  

#Use table to identify odd values of different variables. (Not sure where odd values are printing...)
table(d$pclass)
table(d$embarked)

#Replace all empty strings and NAs in embarked with S.
d$embarked[d$embarked == ""|NA] <- "S"
#Confirm that no empty strings remain in d$embarked.
d$embarked
sum(is.na(d$embarked))
sum(d$embarked == "")

#Is the age column missing any values?
any(is.na(d$age))

#See d$age.
d$age

#See missing age values.
is.na(d$age)

#How many age values are missing?
sum(d$age == "")

#Calculate the mean of the Age column and use that value to populate the missing values
d$age[is.na(d$age)] <- 23.86
d$age

#? QUESTION: 2 strange things here. First I tried to make it 
#? d$age[is.na(d$age)] <- mean(d$age); it ran the code, but NAs still there -- why didn't this work?

#Note mean is taken from summary(d)

# Think about other ways you could have populated the missing values in the age
# column. Why would you pick any of those over the mean (or not)?

# A better way, I think, would be to assign age as a mean based on other
# variables; e.g. mean of ages for: 1st class, survived / 2nd class, did not
# survive / 3rd class survived, etc.
#? How would I do that method?

#Confirm all NA values have been replaced. 
any(is.na(d$age))

#See boat.
d$boat

#How many boat values are missing?
is.na(d$boat)
sum(d$boat == "")

#Fill empty boat slots with a dummy value e.g. the string 'None' or 'NA'
d$boat[d$boat == ""] <- 'NA'

#See boat.
d$boat

# Does it make sense to fill missing cabin numbers with a value? What does a
# missing value here mean?

# My answer: I initially agree with Kaggle commenter, desilu: "I can't see why
# blank versus non-blank makes much difference for survival. It seems that the
# missing entries are indicative of a clerical problem, not someone sitting at
# the lifeboat asking what cabin they came from." But it's also relevant what
# Kaggle commenter small yellow duck said: "It turns out that most of the cabin
# information is for people in first class because a partial first class
# passenger list was recovered on the body of one of the stewards.
# http://www.encyclopedia-titanica.org/cabins.html" This would indicate blank
# cabin entries would be less likely to be first class and more likely to be
# second or third class.

# Create a new column has_cabin_number which has 1 if there is a cabin number, and 0 otherwise.
d$has_cabin_number <- ifelse(d$cabin != "", 1, 0)

# Confirm new column created.
summary(d)

# Note: I notice there is data for less than 25% of cabins.

# Check out final data.
glimpse(d)

titanic_clean <- write.table(d)
write.csv(titanic_clean, file = "titanic_clean.csv")
titanic_clean

