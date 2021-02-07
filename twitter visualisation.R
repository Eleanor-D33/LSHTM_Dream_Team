# exploring presentation of empath analysis results

#load packages
# install.packages("tidyverse")
library(tidyverse)
library(ggplot2)
# install.packages("zoo")
library(zoo) # for moving averages
# install.packages("tidyr")
library(tidyr)

# set working directory 
setwd("C:/Users/Issi/Documents/HDS/Data Challenge")

#read in twitter data
tweet_analysis <- read.csv("empath_analysis_mean_pivot_table.csv")

# change format of data from character to date
tweet_analysis$tw_date <- as.Date(tweet_analysis$tw_date, format="%Y-%m-%d")

#create subset data starting at in Jan 2020
tweet_analysis_2020 <- subset(tweet_analysis, tw_date > as.Date("2020-01-01"))

# calculate 7 day rolling average score for each empath category
tweet_analysis_2020_7da <- tweet_analysis_2020 %>%
  dplyr::mutate(air_travel_07da = zoo::rollmean(air_travel, k = 7, fill = NA),
                banking_07da = zoo::rollmean(banking, k = 7, fill = NA),
                death_07da = zoo::rollmean(death, k = 7, fill = NA),
                divine_07da = zoo::rollmean(divine, k = 7, fill = NA),
                envy_07da = zoo::rollmean(envy, k = 7, fill = NA),
                gain_07da = zoo::rollmean(gain, k = 7, fill = NA),
                independence_07da = zoo::rollmean(independence, k = 7, fill = NA),
                injury_07da = zoo::rollmean(injury, k = 7, fill = NA),
                masculine_07da = zoo::rollmean(masculine, k = 7, fill = NA),
                negotiate_07da = zoo::rollmean(negotiate, k = 7, fill = NA),
                payment_07da = zoo::rollmean(payment, k = 7, fill = NA),
                programming_07da = zoo::rollmean(programming, k = 7, fill = NA),
                reading_07da = zoo::rollmean(reading, k = 7, fill = NA),
                religion_07da = zoo::rollmean(religion, k = 7, fill = NA),
                sleep_07da = zoo::rollmean(sleep, k = 7, fill = NA),
                swearing_terms_07da = zoo::rollmean(swearing_terms, k = 7, fill = NA),
                technology_07da = zoo::rollmean(technology, k = 7, fill = NA),
                tourism_07da = zoo::rollmean(tourism, k = 7, fill = NA),
                weakness_07da = zoo::rollmean(weakness, k = 7, fill = NA),
                worship_07da = zoo::rollmean(worship, k = 7, fill = NA),
                )


# remove raw value columns
shortdata_7da <- subset(tweet_analysis_2020_7da, select = c("tw_date", "air_travel_07da","banking_07da","death_07da","divine_07da","envy_07da","gain_07da","independence_07da","injury_07da","masculine_07da","negotiate_07da","payment_07da","programming_07da","reading_07da","religion_07da","sleep_07da","swearing_terms_07da","technology_07da","tourism_07da","weakness_07da","worship_07da"))

# pivot the table with all categories to make it easier to construct the graph
longdata_7da <- 
  shortdata_7da %>% 
  pivot_longer(
             cols = c("air_travel_07da","banking_07da","death_07da","divine_07da","envy_07da","gain_07da","independence_07da","injury_07da","masculine_07da","negotiate_07da","payment_07da","programming_07da","reading_07da","religion_07da","sleep_07da","swearing_terms_07da","technology_07da","tourism_07da","weakness_07da","worship_07da"),
             names_to = "empath_category",
             values_to = "empath_score_7da")

# line graph of all categories
longdata_7da %>%
  ggplot(aes(x=tw_date,
             y=empath_score_7da,
             group = factor(empath_category)))+
  geom_line(aes(colour=empath_category))+
  labs(title = "Empath Category Score Over Time",
       x = "Date",
       y = "Empath score (7-day rolling-mean)")

# line graphs - facet wrapped
longdata_7da %>%
  ggplot(aes(x=tw_date,
             y=empath_score_7da,
             group = factor(empath_category)))+
  geom_line(aes(colour=empath_category))+
  facet_grid(rows = vars(empath_category),
             switch = "x")
  labs(title = "Empath Category Score Over Time",
       x = "Date",
       y = "Empath score (7-day rolling-mean)")
  
#########################################
###  Positively correlated categories  ##
#########################################  
  
#make a table for positively correlated categories
shortdata_7da_pos <- subset(shortdata_7da, select = c("tw_date", "worship_07da", "masculine_07da", "death_07da", "weakness_07da", "divine_07da", "religion_07da", "sleep_07da", "swearing_terms_07da", "injury_07da", "envy_07da"))

  # pivot the table with all categories to make it easier to construct the graph
  longdata_7da_pos <- 
    shortdata_7da_pos %>% 
    pivot_longer(
      cols = c("worship_07da", "masculine_07da", "death_07da", "weakness_07da", "divine_07da", "religion_07da", "sleep_07da", "swearing_terms_07da", "injury_07da", "envy_07da"),
      names_to = "empath_category",
      values_to = "empath_score_7da")
  
  # line graph of all categories
  longdata_7da_pos %>%
    ggplot(aes(x=tw_date,
               y=empath_score_7da,
               group = factor(empath_category)))+
    geom_line(aes(colour=empath_category))+
    labs(title = "Positively Correlated Empath Categories Score Over Time",
         x = "Date",
         y = "Empath score (7-day rolling-mean)")
  
  # line graphs - facet wrapped
  longdata_7da_pos %>%
    ggplot(aes(x=tw_date,
               y=empath_score_7da,
               group = factor(empath_category)))+
    geom_line(aes(colour=empath_category))+
    facet_grid(rows = vars(empath_category),
               switch = "x")
  labs(title = "Empath Category Score Over Time",
       x = "Date",
       y = "Empath score (7-day rolling-mean)")  
  
#########################################
###  Negatively correlated categories  ##
#########################################  
#make a table for negatively correlated categories
shortdata_7da_neg <- subset(shortdata_7da, select = c("tw_date","gain_07da", "reading_07da", "banking_07da", "independence_07da", "programming_07da", "payment_07da", "technology_07da", "tourism_07da", "air_travel_07da", "negotiate_07da"))
  
  # pivot the table with all categories to make it easier to construct the graph
  longdata_7da_neg <- 
    shortdata_7da_neg %>% 
    pivot_longer(
      cols = c("gain_07da", "reading_07da", "banking_07da", "independence_07da", "programming_07da", "payment_07da", "technology_07da", "tourism_07da", "air_travel_07da", "negotiate_07da"),
      names_to = "empath_category",
      values_to = "empath_score_7da")
  
  # line graph of all categories
  longdata_7da_neg %>%
    ggplot(aes(x=tw_date,
               y=empath_score_7da,
               group = factor(empath_category)))+
    geom_line(aes(colour=empath_category))+
    labs(title = "Negatively Correlated Empath Categories Score Over Time",
         x = "Date",
         y = "Empath score (7-day rolling-mean)")
  
  # line graphs - facet wrapped
  longdata_7da_neg %>%
    ggplot(aes(x=tw_date,
               y=empath_score_7da,
               group = factor(empath_category)))+
    geom_line(aes(colour=empath_category))+
    facet_grid(rows = vars(empath_category),
               switch = "x")
  labs(title = "Empath Category Score Over Time",
       x = "Date",
       y = "Empath score (7-day rolling-mean)")   