library(tidyverse)
library(tidyr)
library(lubridate)
library(ggplot2)
library(dplyr)

###########################################################################
###########################################################################
###                                                                     ###
###                     OVERALL REGRESSION ANALYSIS                     ###
###                                                                     ###
###########################################################################
###########################################################################

df <- read.csv("/Users/eleanordavies/Desktop/HDS_Content/Term_2/Data_Challenge/Final_Zip/Analysis/regression_lm.csv")
# df[1,]
# k <- df[1,]
# names(k)<-NULL
# coefs <- c(0, k[1:length(k)-1])
# model$coefficients[1:21]<- c(coefs)
model <- lm(data = df, 
             formula = outcome ~ worship+ masculine+death+weakness+divine+religion+sleep+swearing_terms+injury+envy+gain+reading+ banking+independence+programming+payment+technology+tourism+air_travel+negotiate)

load("/Users/eleanordavies/Desktop/HDS_Content/Term_2/Data_Challenge/Final_Zip/Tweet_Data/tweets_total_clean.RData")
tweet_analysis <- files
tweet_analysis$pred <- predict(model, tweet_analysis)

#removal of covid tweets (brings up tech category)
tweet_analysis1 <- tweet_analysis %>% dplyr::filter(!label %in% 'API_covid_clean')

tweet_analysis2019 <- tweet_analysis1 %>% filter(Date < "2019-12-31")
tweet_analysis2020 <- tweet_analysis1 %>% filter(Date > "2020-01-01")


pdf("/Users/eleanordavies/Desktop/HDS_Content/Term_2/Data_Challenge/Final_Zip/Analysis/plots/overall_regression_model.pdf", width=10, height=6) 
ggplot() +
  geom_point(data = tweet_analysis2020, aes(Date, pred),linetype=4, color="darkblue") + 
  geom_point(data = tweet_analysis2019, aes(Date, pred),linetype=4, color="darkred") +
  geom_hline(yintercept = max(tweet_analysis2020$pred), linetype=4,color="blue") + 
  geom_hline(yintercept = max(tweet_analysis2019$pred), linetype=4, color="red") +
  geom_vline(xintercept = as.numeric(ymd("2020-03-27")) , linetype=1, colour="black") +
  geom_vline(xintercept = as.numeric(ymd("2020-07-04")), linetype=2, color="black") +
  geom_vline(xintercept = as.numeric(ymd("2020-11-01")) , linetype=1, colour="black") +
  geom_vline(xintercept = as.numeric(ymd("2020-12-02")), linetype=2, color="black") 

dev.off() 


cor.test(tweet_analysis2020$Date, tweet_analysis2020$pred)


 



 

