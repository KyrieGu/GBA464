# # ===================================================
# GBA464: Assignment 1
# Author: Yufeng Huang
# Student: Tianrun Gu
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
population <- as.numeric(population)
numATMs <- as.numeric(numATMs)
ATMwithdr <- as.numeric(ATMwithdr)
withdrvalue <- as.numeric(withdrvalue)
unemprate <-as.numeric(unemprate)
numbranches <- as.numeric(numbranches)



# Q2b. population is in a very different scale. Re-scale it into thousands, i.e., divide population by 1000
#   and replace the variable
population <- population / 1000



# ==== question 3 ====

# You want to take average for all variables but you realized that some variables have missing value
#   before taking averages, you need to make sure that all observations are taken from the same sets of 
#   observations (i.e. rows) where no variable is missing 

# Q3a. let's define a logical vector for non-missing rows, i.e. indicate TRUE for rows without any missing values, 
#   and FALSE for rows with missing values. Name the vector 'nm'. Note that the length of nm will be the same as 
#   the number of rows in the original data df
#define vector nm
nm <- c(population & numATMs & ATMwithdr & withdrvalue & unemprate & numbranches)
nm



# Q3b. count the number of non-missing rows in the data df, name it 'count_nm'
#   count_nm should be one number. Note that there is no such a function 'count'. 
#   Think about how to count the number of non-missing rows. 
non_missing <- nm[nm == TRUE]
count_nm <- length(non_missing)
count_nm




# ==== question 4 ====

# Q4. Calculate the averages of number of ATM, number of branches, population, 
#   unemployment rate, number of withdraw per resident and amount per withdrawl.
#   In particular, notice that certain variables have missing values and you might want to  
#   only calculate means for the rows without missing values of any variable
#   (that is, the rows that you use to calculate the average of all variables should be the same)
#   Finally, collect results in a vector called 'mean_nm', name elements in the vector by the original variable name

#datasets without missing values
non_population <- population[nm]
non_numATM <- numATMs[nm]
non_ATMwithdr <- ATMwithdr[nm]
non_withdrvalue <- withdrvalue[nm]
non_unemprate <- unemprate[nm]
non_numbranches <- numbranches[nm]


#avg of ATM
avg_num_ATM <- sum(non_numATM) / count_nm

#avg of N branches
avg_branches <- sum(non_numbranches) / count_nm

#avg of population
avg_population <- sum(non_population) / count_nm

#avg of unemloyment rate
avg_unemployment <- sum(non_unemprate) / count_nm

#avg of number of withdraw per resident
avg_num_widthdraw <- mean(non_ATMwithdr)

#avg of amount per withdraw
avg_amount <- mean(non_withdrvalue)


#define vector mean_nm
mean_nm <- c(avg_num_ATM,avg_branches,avg_population,avg_unemployment,avg_num_widthdraw,avg_amount)
names(mean_nm) <- c("numATMs","numbranches","population",
                    "unemprate","ATMwithdr","withdrvalue")
mean_nm



# ==== question 5 ====

# Q5. You realize that the reason for missing values in the original data is that there are no ATMs.
#   So in that regard you could have defined the missing values to zero
#   Re-define the missings to zero and assign it to the original variable,
#   find the total number of observations in the dataset (call it 'count_all'), 
#   and re-calculate means for the same set of variables and collect results in 'mean_all'

#redefine missing values
ATMwithdr[!nm] <- 0
withdrvalue[!nm] <- 0

#find the total number of observations
count_all <- length(ATMwithdr)
count_all

#recalculate means
#avg of ATM
avg_num_ATM <- mean(numATMs)

#avg of N branches
avg_branches <- mean(numbranches)

#avg of population
avg_population <- mean(population)

#avg of unemloyment rate
avg_unemployment <- mean(unemprate)

#avg of number of withdraw per resident
avg_num_widthdraw <- mean(ATMwithdr)

#avg of amount per withdraw
avg_amount <- mean(withdrvalue)


#collect results in 'mean_all'
mean_all <- c(avg_num_ATM,avg_branches,avg_population,avg_unemployment,avg_num_widthdraw,avg_amount)
names(mean_all) <- c("numATMs","numbranches","population",
                    "unemprate","ATMwithdr","withdrvalue")
mean_all

# ==== question 6 ====

# You decide to investigate what's the average number of withdrawal and amount per withdrawal
#   by areas with different number of ATMs

# Q6a. Let's summarize average ATMwithdr and average withdrvalue by the number of atms (for range 1-4). 
#   collect results in two separate vectors and name them 'mean_a' and 'mean_w'
mean_a <- c(mean(ATMwithdr[numATMs == 1]), mean(ATMwithdr[numATMs == 2]),
                                                mean(ATMwithdr[numATMs == 3]),
                                                mean(ATMwithdr[numATMs == 4]))

mean_w <- c(mean(withdrvalue[numATMs == 1]), mean(withdrvalue[numATMs == 2]),
                                                mean(withdrvalue[numATMs == 3]),
                                                mean(withdrvalue[numATMs == 4]))

mean_a
mean_w




# Q6b. Separately, plot these by the number of ATMs; label the x axis "number of ATMs" and y axis 
#   "average withdrawl per resident" and "average amount per withdrawl", respectively
#   use line plot by setting type = 'l' as one of the plot function arguments

#range 1-4
ATM_range <- c(1:4)
#convert to integer vector
ATM_range <- as.integer(ATM_range)
ATM_range

#plot average withdrawl per resident
plot(ATM_range,mean_a,type = 'l',xlab = "number of ATMs", 
     ylab = "average withdrawl per resident",main = "average withdrawl per resident")

#plot average amount per wirthdrawl
plot(ATM_range,mean_w,type = 'l',xlab = "number of ATMs", 
     ylab = "average withdrawl per resident", main = "average amount per wirthdrawl")






