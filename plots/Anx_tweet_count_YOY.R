library(tidyverse)
library(tidyr)
library(lubridate)
library(ggplot2)
library(dplyr)

load("/Users/eleanordavies/Desktop/HDS_Content/Term_2/Data_Challenge/Final_zip/tweets_total_clean.RData")
tweet_analysis <- files
API_anx <- tweet_analysis %>% 
  filter(label == "API_anx_clean")

API_anxYoY = API_anx

# Crunch the data
# Put year in its own column for ease of use below
API_anxYoY$year <- year(API_anxYoY$Date) 
# The meat: Create a dummy variable of Jan. 1 of each given year and subtracts this date from the actual date.
API_anxYoY$days <- as.numeric(API_anxYoY$Date - make_date(API_anxYoY$year, 1, 1)) +1 # Add one so Jan. 1 is Day 1 and not Day 0
# Calculate cumulative sum within each year
API_anxYoY <- API_anxYoY %>% group_by(year) %>% arrange(Date) %>% select(Date, count, label,days, year)

# Graph the data
# converting date to day 
day1 <- as.numeric(API_anxYoY[which(API_anxYoY$Date == "2020-03-27"),4][2,1])
day2 <- as.numeric(API_anxYoY[which(API_anxYoY$Date == "2020-07-04"),4][2,1])
day3 <- as.numeric(API_anxYoY[which(API_anxYoY$Date == "2020-11-01"),4][2,1])
day4 <- as.numeric(API_anxYoY[which(API_anxYoY$Date =="2020-12-02"),4][2,1])


pdf("/Users/eleanordavies/Desktop/HDS_Content/Term_2/Data_Challenge/Final_Zip/Analysis/plots/Anx_tweet_count_YOY.pdf", width=10, height=6) 
  ggplot(API_anxYoY, aes(y = count, x= days, group = year, color = as.character(year))) +
  geom_line() +
  geom_vline(xintercept = day1, linetype=1, color="black") +
  geom_vline(xintercept = day2, linetype=2, color="black") +
  geom_vline(xintercept = day3, linetype=1, color="black") +
  geom_vline(xintercept = day4, linetype=2, color="black") +
  theme_minimal()

dev.off() 

API_anxYoY_2020 <- API_anxYoY %>% filter(year == 2020) 
API_anxYoY_2019 <- API_anxYoY %>% filter(year == 2019)

mean(API_anxYoY_2020$count)
mean(API_anxYoY_2019$count)
