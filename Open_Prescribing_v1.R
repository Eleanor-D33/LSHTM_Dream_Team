#rm(list=ls())

##devtools package
#install.packages("devtools")
library(devtools)

##OpenPrescribingR package
#devtools::install_github("fergustaylor/openprescribingR")
library(openprescribingR)

##Tidyverse package
#install.packages("tidyverse")
library(tidyverse)

##Useful documentations
?openprescribingR
?spending_by_code()
?drug_details


########################################
##  Psychiatrics Extraction of Codes  ##
########################################
##-----------------------------------------------------------------
Psychiatrics <- read.csv("chemical_codes.csv")

#Remove white spaces from possible Psychiatric bnf section codes
Psychiatric_Codes <- gsub(" ", 
                          "", 
                          unique(Psychiatrics$bnf_section_code))
##-----------------------------------------------------------------

drug_details()

#########################################
## Anti-Epileptics Extraction of Codes ##
#########################################
##-----------------------------------------------------------------
Antiepileptics <- read.csv("antiepileptic_drug_codes.csv")

#Remove white spaces from possible Antiepileptic bnf section codes
Antiepileptic_Codes <- gsub(" ", 
                            "", 
                            unique(Antiepileptics$bnf_section_code))
##-----------------------------------------------------------------


###################################################
## Function to extract data from OpenPrescribing ##
###################################################
##-----------------------------------------------------------------
Get_Spending <- function(drug_codes) {
  NewTable <- data.frame(actual_cost=double(),
                   date=character(),
                   items=integer(),
                   quantity=integer(),
                   id=character(),
                   stringsAsFactors=FALSE)
  for (val in drug_codes) {
    temp <- spending_by_code(BNF_code = val)
    temp$id <- val
    NewTable<- rbind(NewTable,temp)
  }
  
  return(NewTable)
}
##-----------------------------------------------------------------



########################################
##     Anti-Epileptics Data Pull      ##
########################################
##-----------------------------------------------------------------

#Pull data using Get_Spending Function
Antiepileptic_Data <- Get_Spending(Antiepileptic_Codes)
#Add Antiepileptic Flag
Antiepileptic_Data$Drug_Class <- "AntiEpileptics"

##-----------------------------------------------------------------



########################################
##       Psychiatrics Data Pull       ##
########################################
##-----------------------------------------------------------------

#Pull data using Get_Spending Function
Psychiatric_Data <- Get_Spending(Psychiatric_Codes)
#Add Psychiatric Flag
Psychiatric_Data$Drug_Class <- "Psychiatric"

##-----------------------------------------------------------------



########################################
##         Combine Data Pulls         ##
########################################
##-----------------------------------------------------------------

#Union tables together
All_Data <- rbind(Psychiatric_Data, Antiepileptic_Data)

#Add year column
All_Data$year <- as.numeric(
                    format(as.Date(All_Data$date, format="%Y-%m-%d"),"%Y"))

#Add month column
All_Data$month <- format(as.Date(All_Data$date, format="%Y-%m-%d"),"%m")
All_Data$month <- month.abb[as.numeric(All_Data$month)]

##-----------------------------------------------------------------


Aggregated_Data <- All_Data %>%
  group_by(Drug_Class,year,month) %>%
  summarise(
    Total_Items = sum(items, na.rm = TRUE),
    Total_Cost = sum(actual_cost, na.rm = TRUE),
    Total_Quantity = sum(quantity, na.rm = TRUE)
    
  )

str(Aggregated_Data)

Aggregated_Data %>%
  filter(year >= 2019) %>%
  ggplot(aes(x = month, 
           y = Total_Items, 
           color = Drug_Class, 
           fill = Drug_Class,
           group = Drug_Class)) +
  geom_line() +
  geom_point() +
  facet_grid(rows = vars(year),
           #space = "free_x",  # Let the width of facets vary and force all bars to have the same width.
           switch = "x")  +    # Move the facet labels to the bottom.
  theme(axis.text.x = element_text(angle = 90))
  

RelabelFunc <- function(x) {
  return(paste(as.character(x/(10^6)), "M", sep = ""))
}

Aggregated_Data %>%
  filter(year >= 2018) %>%
  ggplot(aes(x = month, 
         y = Total_Quantity, 
           color = factor(year), 
           #fill = factor(year),
           group = factor(year))) +
  geom_line() +
  geom_point() +
  facet_grid(rows = vars(Drug_Class),
             scales = "free_y",  # Let the width of facets vary and force all bars to have the same width.
             switch = "x")  +    # Move the facet labels to the bottom.
  theme(axis.text.x = element_text(angle = 90),
        plot.title = element_text(hjust = 0.5)) +
  labs(title="Monthly Quantity Dispensed By Drug Class By Year",
       x ="Month", 
       y = "Quantity Dispensed",
       color = "Year") +
  scale_y_continuous(labels=RelabelFunc) +
  scale_x_discrete(limits = month.abb)





Excel_Table <- Aggregated_Data %>%
  select(Drug_Class, year, month, Total_Quantity) %>%
  filter(year >= 2018) %>%
  pivot_wider(names_from = month, values_from = Total_Quantity) %>%
  select(Drug_Class, year, Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov, Dec)