library(tidyverse)

# set working directory to folder containing all csv files to be merged 
wd <- ("/Users/eleanordavies/Desktop/HDS_Content/Term_2/Data_Challenge/Tweet_Data/Meltwater/Anxiety")
files <- list.files(path = wd,
           pattern="*.csv", 
           full.names = T) %>% 
  map_df(~read_csv(.))

as.data.frame(files)

# reading in UTF-16 docs 
df <-read.csv("/Users/eleanordavies/Desktop/csv/Anxiety_2021-02-06-12-47-25.csv", encoding="UTF-16", stringsAsFactors=FALSE)
