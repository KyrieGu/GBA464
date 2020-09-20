# # ===================================================
# GBA464: Assignment 2 
# Author: Yufeng Huang
# Student: Tianrun Gu
# Description: working with data frame
# Data: European car characteristics, prices and sales, 1970-1999
# Source: 
#   cars: https://sites.google.com/site/frankverbo/data-and-software/data-set-on-the-european-car-market
#   crude oil price: OPEC; IEA; extracted from 
#   http://www.statista.com/statistics/262858/change-in-opec-crude-oil-prices-since-1960/
# Optionally, you can also try plotting against UK gasoline price from www.theaa.com 
#   link: https://dl.dropboxusercontent.com/u/13844770/rdata/assignment_2/ukgas.csv
#   (https://www.theaa.com/public_affairs/reports/Petrol_Prices_1896_todate_gallons.pdf)
# Acknowledgement: Frank Verboven has contributed significant effort
#   making the car sales dataset publically available
# ===================================================

# ===== Step 0: load data and required packages =====

# clear everything
rm(list = ls())

#graph tools
library(ggplot2) 
library(tidyverse) 

# download file to local folder if file does not exist
url <- 'https://dl.dropboxusercontent.com/s/nchoevokxmodlqu/cars.csv' 
if (!file.exists('cars.csv')) {     # check whether data exists in local folder (prevents downloading every time)
    download.file(url, 'cars.csv', mode = 'wb')
}
df <- read.csv('cars.csv')  # load data

url <- 'https://dl.dropboxusercontent.com/s/t9z1oe5e4d7uqya/oil.csv' 
if (!file.exists('oil.csv')) {
    download.file(url, 'oil.csv', mode = 'wb')
}
oil <- read.csv('oil.csv')  # load data

# ===== Question 0: what are the keys of the data frames? =====
# Before we start, let's think about what are the keys in the data frame
#   You don't have to do anything now; just think about it
head(oil)
head(df)

# ===== Question 1: cleanup data =====
# 1) Take a subset of df, focusing on observations where class ($cla) is "standard" or 
#   "intermediate" or "luxury", use this sub-data-frame to replace the original df
# 2) Generate a column in df, $mpg, that measures fuel efficiency in mile per gallon
#   note that 1 mile per gallon = 235.215 / (1 liter per 100km). In other words, mpg is not 
#   proportional to liter per 100km, but is proportional to 1/(liter per 100). To check your answers, 
#   keep in mind that we usually see mpgs between 10 and 40. 
#   Also note: in the variable description, $li should be liter per 100km rather than liter per km.
# 3) Find a way to replace year in dataframe oil ($ye) into up to 2 digit (e.g. 1990 becomes 90, and 2001 becomes 1)

#1)
#subset of df --- based on class
df <- df[df$cla == "standard" | df$cla == "intermediate" | df$cla == "luxury",]


#calculate mpg
mpg <- 235.215 / df$li
head(mpg)

#add $mpg column
df <- cbind(df,mpg)
head(df)

#convert year to 2 digits
typeof(oil$ye)
oil$ye <- oil$ye %% 100
head(oil)




# ===== Question 2: summarize fuel efficiency by year and manufacturer =====
# Take average of fuel efficiency $mpg for given the firm ($frm) and year ($ye)
#   You could use the function aggregate()
# Then, plot the average $mpg for firm ($frm) Volkswagen ("VW") across all years.
#   Set your axis label to "year" and "mile per gallon" and put up a title
#   "Volkswagen fuel efficiency"

#calculate average
avg_mpg_by_year <- aggregate(formula = mpg ~ frm + ye, data = df, FUN = mean)
head(avg_mpg_by_year)

# 
# ggplot(avg_mpg_by_year[avg_mpg_by_year$frm == "VW",], aes(ye, mpg)) +
#   geom_point(aes(color = ye)) +
#   labs(
#     x = "year",
#     y = "mile per gallon",
#     title = "Volkswagen fuel efficiency"
#   ) +
#   scale_x_continuous(breaks = seq(70,100, by = 5))

#using plot
plot(x = avg_mpg_by_year$ye[avg_mpg_by_year$frm == "VW"], 
     y = avg_mpg_by_year$mpg[avg_mpg_by_year$frm == "VW"],
     type = 'l',
     col = 3,
     xlab = "year", ylab = "mile per gallon",
     main = "Volkswagen fuel efficiency")




# ===== Question 3: merge with gasoline price data =====
# 1) Merge the average fuel efficiency data with crude oil price data, 
# 2) Create the same plot as above (also for VW) but add a plot of crude oil price over time
#   when doing so:  a) set xlab and ylab to "" in order to not generate any label,
#                   b) generate an axis on the right by using axis(side = 4, ...)
# 3) 1985 was the start of the new US regulation on fuel efficiency (which was announced in 1975).
#   Add a vertical line in the year 1985 to indicate this (search help lty or col for line type or 
#   color; adjust them so that the graph does not look that busy)

#1) Merge avg with oil
avg_with_price <- merge(avg_mpg_by_year,oil, by = "ye")
head(avg_with_price)


#2) 
#accept new plot
par(new = T)
#plot VW graph with oil price
plot(x = avg_mpg_by_year$ye[avg_mpg_by_year$frm == "VW"],
     y = avg_with_price$oilpr[avg_with_price$frm == "VW"],
     type = 'l', xlab = "", ylab = "", 
     col = 4,
     axes=FALSE)
axis(side = 4)
par(new = F)

#3)
#add a vertical line to show 1985
abline(v = 85, col = "red", lty = 2)







# ===== Question 4: find new cars models =====
# Start with the subsetted data focusing on Volkswagen and on "standard" or "intermediate"
#   or "luxury" cars.
# 1) Find the first year where a specific car model (indicated by variable $type) 
#   has positive sales in a given market ($ma); i.e. that's the year when the model started to sell at all
#   Think of this as the year of introduction; consider using aggregate()
# Note: You might want to construct a data frame for this, but in the end merge it with 
#   to the data frame df and assign the merge result to df.augment
# 2) Generate a sub-data frame where each car model just started selling for the first/second year;
#   that is, year <= year of introduction + 1; assign the data frame df.new
# 3) Generate average $mpg by year, for all the cars (in our subset) that started selling for the first/second year; 
#   use aggregate()
# 4) [Optional] Generate the same plot as in Question 3, but now focusing on the "new cars" that we defined above. 


#1) Find the first year where a specific car model has sales
introduction <- aggregate(formula = ye ~ type + ma, data = df[df$frm == "VW",], FUN = min)
head(introduction)

#merge and assign
df.agument <- merge(introduction,df, by = c("type","ma","ye"))
head(df.agument)

#2)sub-data frame
#define a function to filter first two years
#first merge
df.min <- aggregate(formula = ye ~ type, data = df.agument, FUN = min)
temp <- merge(df.min,df[df$frm == "VW",], by = "type")
df.new <- subset(temp, ye.y <= ye.x + 1)
colnames(df.new)[2:3] <- c("Introduction","ye")
head(df.new)

#3)$mpg
avg_by_year <- aggregate(formula = mpg ~ type + ye, data = df.new, FUN = mean)
head(avg_by_year)

#4) plot new cars
par(new = T)
plot(x = df.min$ye,
     y = df.min$mpg,
     type = 'l', xlab = "", ylab = "", 
     col = 7,
     axes=FALSE)
par(new = F)

#add legend to make it clearer
legend("topright", legend = c("All VW", "Price","New Cars"), col = c(3,4,7), pch = 16)

