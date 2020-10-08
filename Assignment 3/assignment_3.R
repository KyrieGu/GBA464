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
  
  #set up the counter
  counter <- 0
  
  #set up the first successful month
  first <- FALSE
  
  #loop through years
  for (y in 1997:1998){
    
    #loop through the months
    for (m in 1:12) {
      if(!first){
        df$recency[df$id == id & df$year == y & df$month == m] <- NA
      } 
      else{
        counter <- counter + 1
        df$recency[df$id == id & df$year == y & df$month == m] <- counter
      }
      
      if(length(df[df$id == id & df$year == y & df$month == m, "trips"]) > 0) {
        if(df[df$id == id & df$year == y & df$month == m, "trips"] > 0){
          first <- TRUE
          counter <- 0
        }
      }
    }  
  }
}
  


# ====== Section 3.2: frequency ======
# first define quarters and collapse/merge data sets
#   quarters should be e.g. 1 for January-March, 1997, 2 for April-June, 1997, ...
#   and there should be 6 quarters in the two-year period
#   Next, let's define frequency purchase occasions in PAST QUARTER
#   Call this df$frequency

#define quarter list
q <- list(c(1,2,3), c(4,5,6), c(7,8,9), c(10,11,12))
df$quarter[df$year == 1997 & df$month == q[[1]]] <- 1
df$quarter[df$year == 1997 & df$month == q[[2]]] <- 2
df$quarter[df$year == 1997 & df$month == q[[3]]] <- 3
df$quarter[df$year == 1997 & df$month == q[[4]]] <- 4
df$quarter[df$year == 1998 & df$month == q[[1]]] <- 5
df$quarter[df$year == 1998 & df$month == q[[2]]] <- 6
names(q) <- c("q1","q2","q3","q4")

#construct df$recency
df$frequency <- NA



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
df$monvalue <- NA
for(id in 1:max(df$id)){
  
  #count how many months has made purchased
  count <- 0
  
  #initialize average
  average <- 0
  
  #initialize the sum
  s <- 0
  
  for (y in c(1997,1998)) {
    #loop through every month
    for(m in 1:12){
      if (average > 0) {
        df$monvalue[df$id == id & df$year == y & df$month == m] <- average 
      }
      
      if(length(df[df$id == id & df$year == y & df$month == m, "expd"]) > 0){
        if(df[df$id == id & df$year == y & df$month == m, "expd"] > 0) {
          count <- count + 1
          s <- s + df$expd[df$id == id & df$year == y & df$month == m]
        }
      }
      if(count > 0){
        #calculate average
        average <- s / count
      }
    }
  }
}





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
#sort
df_sort <- df[order(df$index),]
#Split into 10 groups 
quant <- quantile(df_sort$index,probs = seq(0,1,0.1), na.rm = TRUE)
quant[1]
for (i in 1:(length(quant)-1)) {
  left <- quant[[i]]
  right <- quant[[i+1]]
  df_sort$group[df_sort$index > left & df_sort$index < right] <- i
}

#calculate the average expenditure
avg_expd <- aggregate(expd ~ group, data = df_sort, FUN = mean)
avg_expd

#plot
barplot(expd ~ group, data = avg_expd,
        xlab = "deciles in the RFM indexes", ylab = "Average Expenditure",
        main = "Average Expenditure by Deciles in RFM Index",
        ylim = c(0,max(avg_expd$expd)))

