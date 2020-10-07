# # ===================================================
# GBA464: RFM analysis on CDNOW data
# Author: Yufeng Huang
# Student: Tianrun Gu
# Description: Lab on functions and loops
# Data: CDNOW customer data (this time full data)
# Source: provided by Professor Bruce Hardie on
#   http://www.brucehardie.com/datasets/CDNOW_sample.zip
# ===================================================

# ====== CLEAR EVERYTHING ======
rm(list = ls())

# ====== READ TRIAL DATA =======

url <- 'https://dl.dropboxusercontent.com/s/xxfloksp0968mgu/CDNOW_sample.txt'
if (!file.exists('CDNOW_sample.txt')) {     # check whether data exists in local folder (prevents downloading every time)
    download.file(url, 'CDNOW_sample.txt')
}
df.raw <- read.fwf('CDNOW_sample.txt', width = c(6, 5, 9, 3, 8), stringsAsFactors = F)  # load data

# ====== Section 2: loading the data ======

df.raw[[1]] <- NULL # drop old id
names(df.raw) <- c("id", "date", "qty", "expd")

# a) generate year and month
#clean up the data
df.raw <- na.omit(df.raw)
#convert date to string
df.raw$date <- as.character(df.raw$date)

#get the year
df.raw$year <- substring(df.raw$date, 1,4)

#get the month
df.raw$month <- substring(df.raw$date,5,6)

#preview the data
head(df.raw)

# b) aggregate into monthly data with number of trips and total expenditure
year_level <- aggregate(cbind(qty, expd) ~ id + year + month, data = df.raw, FUN = sum)
#construct unique function
length.unique <- function(x) length(unique(x))

#calculate the counts
frequency <- aggregate(expd ~ id + year + month, data = df.raw, FUN = length)
#rename to "trips"
colnames(frequency)[4] <- "trips"

#combine to a new data frame
df <- merge(year_level,frequency, by = c("id","year","month"), all = TRUE)
#sort
df <- df[with(df,order(id)),]
#result
head(df)


# c) generate a table of year-months, merge, replace no trip to zero.
# Hint: how do you deal with year-months with no trip? These periods are not in the original data,
#   but you might need to have these periods when you calculate RFM, right?
# Consider expanding the time frame using expand.grid() but you do not have to.
#construct table of year-months
df_new <- expand.grid(id = seq(1,1000,1), year = 1997, month = seq(01,12,1))
df_new <- merge(df_new, expand.grid(id = seq(1,1000,1), year = 1998, month = seq(1,6,1)),
                by = c("id","year","month"), all = TRUE)

#format month in df
df$month <- as.integer(df$month)

#merge it with our data frame
df <- merge(df,df_new, by = c("id","year","month"), all = TRUE)

#sort again
df <- df[with(df,order(id,year,month)),]

#replace all NA with 0
df[is.na(df)] <- 0
#result
head(df)

# now we should have the dataset we need; double check to make sure that every consumer is in every period


# ====== Section 3.1: recency ======
# use repetition statement, such as a "for-loop", to generate a recency measure for each consumer 
#   in each period. Hint: if you get stuck here, take a look at Example 3 when we talked about "for-loops"
#   call it df$recency

#construct df$recency
df$recency <- NA
#loop through every id
for (id in 1:max(df$id)){
  #loop through years
  for (y in 1997:1998){
    #set up the current successful month
    current = 0
    #loop through the months
    for (m in 1:12) {
      if(current == 0){
        df$recency[df$id == id & df$year == y & df$month == m] <- NA
      } 
      else{
        df$recency[df$id == id & df$year == y & df$month == m] <- m - current
      }
      
      if(length(df[df$id == id & df$year == y & df$month == m, "trips"]) > 0) {
        if(df[df$id == 1 & df$year == y & df$month == m, "trips"] > 0){
          #move the pointer
          current = m
        }
      }
    }  
  }
}
  


# ====== Section 3.2: frequency ======
# first define quarters and collapse/merge data sets
#   quarters should be e.g. 1 for January-March, 1997, 2 for April-June, 1997, ...
#   and there should be 8 quarters in the two-year period
#   Next, let's define frequency purchase occasions in PAST QUARTER
#   Call this df$frequency

#construct df$recency
df$frequency <- NA

#define quarter list
q <- list(c(1,2,3), c(4,5,6), c(7,8,9), c(10,11,12))
names(q) <- c("q1","q2","q3","q4")

#loop through every id
for (id in 1:max(df$id)){
  #loop through years
  #Initialize the very first
  first <- TRUE
  for (y in 1997:1998){
    #set up the current successful month
    current = 0
    #loop through the list every quarter
    #sum
    sum <- 0
    for (l in q) {
      temp <- 0
      #loop every month in a quarter
      for(m in l){
        #if it's not the first quarter
        if(first){
          df$frequency[df$id == id & df$year == y & df$month == m] <- NA
        } else{
          df$frequency[df$id == id & df$year == y & df$month == m] <- sum
        }
        #sum current quarter for the next
        temp <- temp + df$trips[df$id == id & df$year == y & df$month == m]
      }
      sum <- temp
      if(!is.null(sum)){
        first <- FALSE
      }
    }  
  }
}





# ====== Section 3.3: monetary value ======
# average monthly expenditure in the months with trips (i.e. when expenditure is nonzero)
#   for each individual in each month, find the average expenditure from the beginning to 
#   the PAST MONTH. Call this df$monvalue








# ====== Section 4: Targeting using RFM ======
# now combine these and construct an RFM index
#   You only need to run this section.

b1 <- -0.05
b2 <- 3.5
b3 <- 0.05

df$index <- b1*df$recency + b2*df$frequency + b3*df$monvalue


# validation: check whether the RFM index predict customer purchase patterns
# Order your sample (still defined by keys of consumer-year-month) based on the RFM index. 
#   Split your sample into 10 groups. The first group is top 10% in terms of
#   the RFM index; second group is 10%-20%, etc.
# Make a bar plot on the expected per-trip revenue that these consumers generate and comment on 
#   whether the RFM index help you segment which set of customers are "more valuable"





