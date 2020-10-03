# # ===================================================
# GBA464: Assignment 1
# Author: Yufeng Huang
# Description: location choice of Belgium ATMs
# Data: Belgium ATM distribution in 1994
# Source: will reveal later
# ===================================================
# INSTRUCTIONS
# 1. this assignment is individual and you cannot look at others' code
# 2. this assignment is graded mostly on correctness of the results but please do 
#       try to maintain readability; please also follow the variable naming instructions
# 3. variable definitions are downloadable at 
#   https://dl.dropboxusercontent.com/s/i5mb8saii14fa6h/variable_definition.txt
# 4. please make sure your code runs from the start to the end and produces the intended results

# clear everything
rm(list = ls())

# load data
url <- 'https://dl.dropboxusercontent.com/s/q6qzbfa1tdcqv6v/belgium_atm.csv'
df <- read.csv(url, stringsAsFactors = F)

# we can check the structure of the data by running

head(df)

# ==== question 1 ====

# Q1. First, recall that df is a data frame which is like a spreadsheet in Excel. 
#   Let's convert every column into a separate variable using '$'; for example:
population <- df$population
numATMs <- df$numATMs

# do the same for the other columns
ATMwithdr <- df$ATMwithdr
withdrvalue <- df$withdrvalue
unemprate <- df$unemprate
numbranches <- df$numbranches





# ------ you should not work with 'df' anywhere beyond this line -----
#   i.e. please only work with the vectors (or create new vectors)
#   for the rest of this assignment


# ==== question 2 ====

# Q2a. Do the necessary conversion for all variables so that you can apply numeric operations on them
#   replace the original value of the vector in case of conversion

# By class() or typeof() we can check if it is numeric or not
class(ATMwithdr)
class(withdrvalue)
class(unemprate)
class(numbranches)
class(population)
class(numATMs)

# Now we can see that ATMwithdr and withdrvalue are not numeric so we convert them

ATMwithdr <- as.double(ATMwithdr)
class(ATMwithdr) # We can see that ATMwithdr is now numeric
withdrvalue <- as.double(withdrvalue)
class(withdrvalue) # We can see that withdrvalue is now numeric


# Q2b. population is in a very different scale. Re-scale it into thousands, i.e., divide population by 1000
#   and replace the variable

population <- population/1e3 # 1e3 is just faster way of writing 1000. Same way, 1e4 is 10000, etc., 




# ==== question 3 ====

# You want to take average for all variables but you realized that some variables have missing value
#   before taking averages, you need to make sure that all observations are taken from the same sets of 
#   observations (i.e. rows) where no variable is missing 

# Q3a. let's define a logical vector for non-missing rows, i.e. indicate TRUE for rows without any missing values, 
#   and FALSE for rows with missing values. Name the vector 'nm'. Note that the length of nm will be the same as 
#   the number of rows in the original data df

# We saw that the all vectors except ATMwithdr and withdrvalue are numeric, therefore, they didn't have any missing values.
# But these two vectors had missing values and after the conversion, the characters were stored as NA
# By using function is.na(), we can find the locations of these NA values.

# The result will be the logical vector like [True False True True etc.] where TRUE values represent the NA locations.

# Since question is asking the rows without ANY missing values, we need to find a 
# logical vector which shows non-missing rows for all of our vectors
# It can be done by using AND (&) or OR (|) operators. 
# !is.na(ATMwithdr) will give non-missing rows for the first vector and
# !is.na(withdrvalue) will show non-missing rows for the second vector
# is.na() gives TRUE for the missing rows, but by using exclamation mark (!) in 
# front of it, we will reverse the result. Therefore, !is.na() outputs TRUE
# for the non-missing rows.
# Then, we combine these results by using AND (&) operator. The output will be 
# TRUE values when both vectors have non-missing values. If at least one of them
# have missing value, then the AND (&) operator will output FALSE. 
nm <- !is.na(ATMwithdr) & !is.na(withdrvalue)
head(nm) # we can partially check it by printing first 6 rows of the vector and comparing it to original dataframe



# Q3b. count the number of non-missing rows in the data df, name it 'count_nm'
#   count_nm should be one number. Note that there is no such a function 'count'. 
#   Think about how to count the number of non-missing rows. 

# Since nm has TRUE values only for the non-missing rows, we just need to count 
# the number of TRUEs in the nm.
# To count TRUEs, we can just use function sum() on the vector nm.
# This is because TRUE is seen by the machine as 1, and FALSe is seen as 0.
# Therefore, we can directly use sum(nm)
count_nm <- sum(nm)
count_nm # Don't forget this row to print the output (easier for the TA to check)




# ==== question 4 ====

# Q4. Calculate the averages of number of ATM, number of branches, population, 
#   unemployment rate, number of withdraw per resident and amount per withdrawl.
#   In particular, notice that certain variables have missing values and you might want to  
#   only calculate means for the rows without missing values of any variable
#   (that is, the rows that you use to calculate the average of all variables should be the same)
#   Finally, collect results in a vector called 'mean_nm', name elements in the vector by the original variable name

# The question is asking to calculate mean values ONLY for the non-missing rows.
# We already have the locations of the rows without any missing values. They are 
# stored in the logical vector nm. 
# We can use it to create a subset of only non-missing rows in the vector. 
# The vector can be subsetted by just using []. 
# For example, population[nm] will create a subset of the vector population, 
# based on the TRUE or FALSE values. That is, whichever row with TRUE will be 
# included in the subset and rows with FALSE will be ignored. Note that it 
# doesn't change the original vector population, it will just create a new subset.
# Now you can apply function mean() to calculate the mean values of this subset.


mean_population <- mean(population[nm])
mean_numATMs <- mean(numATMs[nm])
mean_ATMwithdr <- mean(ATMwithdr[nm])
mean_withdrvalue <- mean(withdrvalue[nm])
mean_unemprate <- mean(unemprate[nm])
mean_numbranches <- mean(numbranches[nm])

# Don't forget to name the columns as asked by the question
mean_nm <- c(population = mean_population, numATMs = mean_numATMs, ATMwithdr = mean_ATMwithdr,
             withdrvalue = mean_withdrvalue, unemprate = mean_unemprate, numbranches = mean_numbranches)
mean_nm  # Don't forget this row to print the output (easier for the TA to check)






# ==== question 5 ====

# Q5. You realize that the reason for missing values in the original data is that there are no ATMs.
#   So in that regard you could have defined the missing values to zero
#   Re-define the missings to zero and assign it to the original variable,
#   find the total number of observations in the dataset (call it 'count_all'), 
#   and re-calculate means for the same set of variables and collect results in 'mean_all'

# It is already said in the question that the vectors ATMwithdr and withdrvalue 
# have the missing values because there are simply no ATMs in these locations. 
# So we know that the missing values for these two vectors are at the same 
# locations. We need to assign 0 for these rows 

# Again, we can just subset these missing rows by using []. Since nm represents
# non-missing rows, it's reverse !nm will represent missing rows.
# Note that now you changed the original vector by applying 0 to rows with NA
ATMwithdr[!nm] <- 0
withdrvalue[!nm] <- 0
count_all <- length(ATMwithdr) # Total number of rows is just length of any vector
count_all  # Don't forget this row to print the output (easier for the TA to check)

# After assigning 0 to the missing rows, we can now directly calculate total 
# average values of the vectors. (It was impossible before due to NA rows)
mean_all_population <- mean(population)
mean_all_numATMs <- mean(numATMs)
mean_all_ATMwithdr <- mean(ATMwithdr)
mean_all_withdrvalue <- mean(withdrvalue)
mean_all_unemprate <- mean(unemprate)
mean_all_numbranches <- mean(numbranches)

# Again, let's not forget to name the columns
mean_all <- c(population = mean_all_population, numATMs = mean_all_numATMs, ATMwithdr = mean_all_ATMwithdr,
             withdrvalue = mean_all_withdrvalue, unemprate = mean_all_unemprate, numbranches = mean_all_numbranches)
mean_all  # Don't forget this row to print the output (easier for the TA to check)



# ==== question 6 ====

# You decide to investigate what's the average number of withdrawal and amount per withdrawal
#   by areas with different number of ATMs

# Q6a. Let's summarize average ATMwithdr and average withdrvalue by the number of atms (for range 1-4). 
#   collect results in two separate vectors and name them 'mean_a' and 'mean_w'

# Question might be tricky to understand, but essentially it asks to calculate
# mean values for the locations with number of ATMs equal to 1, 2, 3 and 4.

# Same as before, we can subset by using []. But which vectors to use for subset?
# The condition will be the number of ATMs. 
# numATMs == 1 is comparison and it will give TRUE for the numATMs = 1 and FALSE
# otherwise. We can then directly use this logical vector to create a subset and 
# calculate the mean values only for this subset. same with other number of ATMs

mean_a <- c(one_atm = mean(ATMwithdr[numATMs==1]), two_atms = mean(ATMwithdr[numATMs==2]),
            three_atms = mean(ATMwithdr[numATMs==3]), four_atms = mean(ATMwithdr[numATMs==4]))
mean_a  # Don't forget this row to print the output (easier for the TA to check)

mean_w <- c(one_atm = mean(withdrvalue[numATMs==1]), two_atms = mean(withdrvalue[numATMs==2]),
            three_atms = mean(withdrvalue[numATMs==3]), four_atms = mean(withdrvalue[numATMs==4]))
mean_w  # Don't forget this row to print the output (easier for the TA to check)







# Q6b. Separately, plot these by the number of ATMs; label the x axis "number of ATMs" and y axis 
#   "average withdrawl per resident" and "average amount per withdrawl", respectively
#   use line plot by setting type = 'l' as one of the plot function arguments

# For the plot, we need x and y values. x values here are the number of ATMs 
# corresponding to previously calculated averages. So x is just simply 1 to 4 
numatmsrange = 1:4
plot(numatmsrange, mean_a,
     xlab = "Number of ATMs", ylab = "Average withdrawal per resident",
     main = "Average withdrawal per resident vs number of ATMs", type = 'l')
plot(numatmsrange, mean_w,
     xlab = "Number of ATMs", ylab = "Average amount per withdrawal",
     main = "Average amount per withdrawal vs number of ATMs", type = 'l')






