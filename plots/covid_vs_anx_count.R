library(tidyverse)
library(tidyr)
library(lubridate)
library(ggplot2)
library(dplyr)

#reading in data 
drug <- read.csv("/Users/eleanordavies/Desktop/HDS_Content/Term_2/Data_Challenge/Final_Zip/Analysis/items for antidepressant drugs per.csv")

drug$date <- as.Date(drug$date)

drug <- drug %>% 
  select(date, y_items) %>% 
  group_by(date) %>% 
  summarise(drug_count = sum(y_items))

load("/Users/eleanordavies/Desktop/HDS_Content/Term_2/Data_Challenge/Final_Zip/tweets_total_clean.RData")
tweet_analysis <- files

unique(tweet_analysis$label)
API_anx <- tweet_analysis %>% 
  filter(label == "API_anx_clean") %>% 
  select(Date, count)
API_covid <- tweet_analysis %>% 
  filter(label == "API_covid_clean") %>% 
  select(Date, count)
API_general <- tweet_analysis %>% 
  filter(label == "API_general_clean") %>% 
  select(Date, count)
mw_suicide <- tweet_analysis %>% 
  filter(label == "mw_suicide_clean" )%>% 
  select(Date, count)
mw_anx <-tweet_analysis %>% 
  filter(label == "mw_anx_clean") %>% 
  select(Date, count)


#################################################################
##             Graph 1a: Volume of tweets over time            ##
#################################################################
tweet_analysis_filtered <- tweet_analysis %>% 
  filter(label %in% c("API_anx_clean", "API_covid_clean") & Date > "2020-01-01")

pdf("/Users/eleanordavies/Desktop/HDS_Content/Term_2/Data_Challenge/Final_Zip/Analysis/plots/covid_vs_anx_count.pdf", width=10, height=6) 

ggplot(tweet_analysis_filtered, aes(Date, count, color = label)) + 
  geom_line() +
  geom_vline(xintercept = as.numeric(ymd("2020-03-27")) , linetype=1, colour="black") +
  geom_vline(xintercept = as.numeric(ymd("2020-07-04")), linetype=2, color="black") +
  geom_vline(xintercept = as.numeric(ymd("2020-11-01")) , linetype=1, colour="black") +
  geom_vline(xintercept = as.numeric(ymd("2020-12-02")), linetype=2, color="black") 

dev.off() 
