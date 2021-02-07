library(tidyverse)

# set working directory to folder containing all csv files to be merged 
wd <- ("/Users/eleanordavies/Desktop/csvfiles")
files <- list.files(path = wd,
           pattern="*.csv", 
           full.names = T) %>% 
  map_df(~read_csv(.))

as.data.frame(files)

