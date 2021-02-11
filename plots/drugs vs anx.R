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
##           Graph 1c: Correlation tweets with drugs           ##
#################################################################

# grouping drug count by month and year 
drug1 <- drug %>%
  dplyr::mutate(year = lubridate::year(date), 
                month = lubridate::month(date)) %>% 
  select(year, month, drug_count) %>% 
  group_by(year, month) %>% 
  summarise(drug_count = sum(drug_count))

## Correlation for overall dataset 
# grouping tweets by months and year and sum count 
tweet_analysis1 <- tweet_analysis %>%
  dplyr::mutate(year = lubridate::year(Date), 
                month = lubridate::month(Date)) %>% 
  select(year, month, count, label) %>% 
  group_by(year, month, label) %>% 
  summarise(tweet_count = sum(count))

# Merging tweet and drug counts 
# tweet_drug <-  tweet_analysis1 %>% merge(drug1, by=c("year","month"))

## Correlation for mw anx ds 
# grouping tweets by months and year and sum count 
API_anx1 <- API_anx %>%
  dplyr::mutate(year = lubridate::year(Date), 
                month = lubridate::month(Date)) %>% 
  select(year, month, count) %>% 
  group_by(year, month) %>% 
  summarise(tweet_count = sum(count))

# Merging tweet and drug counts 
anx_drug <-  drug1 %>% merge(API_anx1, by=c("year","month"))

# removing outlier 
anx_drug <- slice(anx_drug, -17)

# Plotting correlation 
pdf("/Users/eleanordavies/Desktop/HDS_Content/Term_2/Data_Challenge/Final_Zip/Analysis/plots/drugs_v_anx.pdf", width=10, height=6) 
ggplot() +
  geom_point(data = anx_drug, aes(tweet_count, drug_count, colour = 'red')) +
  geom_smooth()
dev.off() 

cor.test(anx_drug$tweet_count, anx_drug$drug_count)
