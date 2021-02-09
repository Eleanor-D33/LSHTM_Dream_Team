# load file 
setwd("/Users/eleanordavies/Desktop/HDS_Content/Term_2/Data_Challenge/Tweet_Data")


mw_suicide_clean <- read.csv("mw_suicide_clean.csv")
mw_suicide_wloc_clean <- read.csv("mw_suicide_wloc_clean.csv")
mw_lockdown_clean <- read.csv("mw_lockdown_clean.csv")
mw_lockdown_wloc_clean <- read.csv("mw_lockdown_wloc_clean.csv")
mw_anx_clean <- read.csv("mw_anx_clean.csv")
mw_anx_wloc_clean <- read.csv("mw_anx_wloc_clean.csv")
API_general_clean <- read.csv("API_general_clean.csv")
API_general_wloc_clean <- read.csv("API_general_wloc_clean.csv")
API_anx_clean <- read.csv("API_anx_clean.csv")
API_anx_wloc_clean <- read.csv("API_anx_wloc_clean.csv")
API_covid_clean <- read.csv("API_covid_clean.csv")
API_covid_wloc_clean <- read.csv("API_covid_wloc_clean.csv")

# Adding label column 
mw_suicide_clean$label <- "mw_suicide_clean"
mw_suicide_wloc_clean$label <- "mw_suicide_wloc_clean"
mw_lockdown_clean$label <- "mw_lockdown_clean"
mw_lockdown_wloc_clean$label <- "mw_lockdown_wloc_clean"
mw_anx_clean$label <- "mw_anx_clean"
mw_anx_wloc_clean$label <- "mw_anx_wloc_clean"
API_general_clean$label <- "API_general_clean"
API_general_wloc_clean$label <- "API_general_wloc_clean"
API_anx_clean$label <- "API_anx_clean"
API_anx_wloc_clean$label <- "API_anx_wloc_clean"
API_covid_clean$label <- "API_covid_clean"
API_covid_wloc_clean$label <- "API_covid_wloc_clean"



# formatting date 
mw_suicide_clean$Date <- as.Date(mw_suicide_clean$Date)
mw_suicide_wloc_clean$Date <- as.Date(mw_suicide_wloc_clean$Date)
#mw_lockdown_clean$Date <- as.Date(mw_lockdown_clean$Date)
#mw_lockdown_wloc_clean$Date <- as.Date(mw_lockdown_wloc_clean$Date)
mw_anx_clean$Date <- as.Date(mw_anx_clean$Date)
mw_anx_wloc_clean$Date <-  as.Date(mw_anx_wloc_clean$Date)
API_general_clean$Date <- as.Date(API_general_clean$Date)    
API_general_wloc_clean$Date <- as.Date(API_general_wloc_clean$Date)
API_anx_clean$Date <- as.Date(API_anx_clean$Date)
API_anx_wloc_clean$Date <- as.Date(API_anx_wloc_clean$Date)
API_covid_clean$Date <- as.Date(API_covid_clean$Date)
API_covid_wloc_clean$Date <- as.Date(API_covid_wloc_clean$Date)


# save files 
write.csv(mw_suicide_clean, "Labelled/mw_suicide_clean.csv")
#write.csv(mw_suicide_wloc_clean, "Labelled/mw_suicide_wloc_clean.csv")
#write.csv(mw_lockdown_clean, "Labelled/mw_lockdown_clean.csv")
#write.csv(mw_lockdown_wloc_clean, "Labelled/mw_lockdown_wloc_clean.csv")
write.csv(mw_anx_clean, "Labelled/w_anx_clean.csv")
#write.csv(mw_anx_wloc_clean, "Labelled/mw_anx_wloc_clean.csv")
write.csv(API_general_clean, "Labelled/API_general_clean.csv")
#write.csv(API_general_wloc_clean, "Labelled/API_general_wloc_clean.csv")
write.csv(API_anx_clean, "Labelled/API_anx_clean.csv")
#write.csv(API_anx_wloc_clean, "Labelled/API_anx_wloc_clean.csv")
write.csv(API_covid_clean, "Labelled/API_covid_clean.csv")
#write.csv(API_covid_wloc_clean, "Labelled/API_covid_wloc_clean.csv")

