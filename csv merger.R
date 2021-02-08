library(tidyverse)

# set working directory to folder containing all csv files to be merged 
wd <- ("/Users/eleanordavies/Desktop/HDS_Content/Term_2/Data_Challenge/Tweet_Data/Meltwater/Anxiety")
files <- list.files(path = wd,
           pattern="*.csv", 
           full.names = T) %>% 
  map_df(~read_csv(.))

as.data.frame(files)

write.csv(files,  file = "/Users/eleanordavies/Desktop/HDS_Content/Term_2/Data_Challenge/Tweet_Data/Meltwater/Anxiety/meltwater_anxiety.csv")

df <-read.csv("/Users/eleanordavies/Desktop/csv/Anxiety_2021-02-06-12-47-25.csv", encoding="UTF-16", stringsAsFactors=FALSE)


csv_file <- "/Users/eleanordavies/Desktop/csv/Anxiety_2021-02-06-12-47-25.csv"


read.csv(
  csv_file, skip = 1, fileEncoding = "UTF-16", sep = "\t", header = FALSE
)
wd1 <- "/Users/eleanordavies/Desktop/csv"
files1 <- list.files(path = wd,
                              pattern="*.csv", 
                              full.names = T) %>% 
  map_df(~read_csv(., skip = 1, fileEncoding = "UTF-16", sep = "\t", header = FALSE))

as.data.frame(files1)