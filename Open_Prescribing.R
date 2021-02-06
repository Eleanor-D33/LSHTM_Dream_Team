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

#### Did not remove quantity from data only the app.
####  it can not be selected but we have it in the code
####  processing here just in case 


###################################################
## Function to extract data from OpenPrescribing ##
###################################################
##-----------------------------------------------------------------
Get_Spending <- function(drug_codes) { #function input is a single drug code
  #can be BNF Chapter, Section, or Paragraph
  
  NewTable <- data.frame(            #Create empty df (will append rows)
    actual_cost=double(),  #actual_cost (dollars spent)
    date=character(),      #date (month in YYYY-MM-DD)
    items=integer(),       #items (prescription volume)
    quantity=integer(),    #quantity (quantity dispensed)
    id=character(),        #id (BNF Chapter, Section, or Paragraph)
    stringsAsFactors=FALSE) #keep strings as strings
  
  #iterate for all ids
  for (val in drug_codes$id) {      
    
    #some ids have no data, so catch errors for empty codes  
    ID_Attempt <- 
      try({temp <- spending_by_code(BNF_code = val)}, #use openPrescribingR function to pull data
          silent = TRUE) #keep executing code if error caught
    
    #if error was caught above, we do not want to append the data to NewTable
    if (class(ID_Attempt) == "try-error") { 
      print(paste(val, " has no data")) #print out message that id has no data
    }
    else{
      temp$id <- val #add id to temp df (the one we currently our iterating through)
      NewTable <- rbind(NewTable,temp) #append to our NewTable df
    }
  }
  
  
  #Set var date to date type using YYYY-mm-dd format
  NewTable$Date <- as.Date(NewTable$date, format="%Y-%m-%d")
  
  #Add year column
  #1. extract year from through format function
  #2. make numeric 
  NewTable$year <- as.numeric( #2
    format(NewTable$Date, #1
           "%Y") #2
  )
  
  
  #Add month column
  #1. extract month from through format function
  #2. make numeric
  #3  use month.abb to change to 3 letter month abbreviation
  NewTable$month <- 
    month.abb[  #3
      as.numeric(  #2
        format(NewTable$Date, #1
               "%m") #2
      )]
  
  NewTable %>%
    rename(
      
    )
  
  
  return(NewTable)
}
##-----------------------------------------------------------------


###########################################
##      ALL POTENTIAL IDS FOR COUNTS     ##
###########################################

#based on OpenPrescribingR documentation, all id's (BNF Chapter, Section, or Paragraph)
#will be pulled since they contain numbers 
Drug_Detail_Table <- drug_details(
  chemical_section_or_presentation_code = c(1,2,3,4,5,6,7,8,9,0)
)

#Dictionary of all id's (BNF Chapter, Section, or Paragraph)
BNF_Dictionary <- 
  Drug_Detail_Table %>% #data from drug detail pull (openPrescribingR function)
  select(     #select these three columns (all we will need)
    id,       #BNF code (BNF Chapter, Section, or Paragraph)
    name,     #description for BNF code
    type) %>% #Most granular level of BNF code
  filter(type %in% c("BNF chapter","BNF section", "BNF paragraph")) %>%
  unique      #ensures no duplication of codes/name/type 

#Run code to extract all BNF codes (BNF Chapter, Section, or Paragraph)
All_Raw_Data_Pull <- Get_Spending(BNF_Dictionary) #takes a little over 6 mins 

#Add in details for name (description for BNF code) and type (Most granular level of BNF code)
Final_Raw_Data_Table <- left_join(All_Raw_Data_Pull, BNF_Dictionary, by = "id")  
rm(All_Raw_Data_Pull) #remove no-longer used table

#Renaming BNF Dictionary for easier reading (had to keep for OpenPrescribingR)
BNF_Dictionary <- BNF_Dictionary %>%
  rename(
    ID = id,
    Description = name,
    BNF_Level = type
  )


##-----------------------------------------------------------------

########################################
##      Aggregated Data Table         ##
########################################
##-----------------------------------------------------------------

#Renaming Columns
Final_Raw_Data_Table <- 
  Final_Raw_Data_Table %>%
  select(id, name, type, Date, year, month, items, quantity, actual_cost) %>% #removes date col
  rename(
    Prescriptions = items, #total prescription volume 
    Cost = actual_cost, #total dollars spent
    Quantity = quantity, #total quantity dispensed 
    ID = id, #id (BNF Chapter, Section, or Paragraph)
    Description = name, #description for BNF code
    BNF_Level = type, #Most granular level of BNF code
    Year = year, #year
    Month = month #month
  )

########################################
##      Graph        ##
########################################
##-----------------------------------------------------------------


# RelabelFunc <- function(x) {
#   return(paste(as.character(x/(10^6)), "M", sep = ""))
# }
BNF_Dictionary

###ENSURING WE HAVE CORRECT # OF ROWS FOR ANALYSIS WITH LAG FUNCTION

#Get all unique descriptions
All_Descriptions <- Final_Raw_Data_Table %>%
  select(Description) %>%
  unique()

#Get all unique dates
All_Dates <- Final_Raw_Data_Table %>%
  select(Date) %>%
  unique()

#cartesian join the two to get all date/description combos
All_Descriptions_With_Dates <- merge(All_Descriptions,All_Dates, all=TRUE)

#join the date/description combos back to the data
Final_Raw_Data_Table_Even_Rows <- 
  left_join(All_Descriptions_With_Dates, Final_Raw_Data_Table, by= c("Description", "Date"))

#remove table
rm(Final_Raw_Data_Table)

names(Final_Raw_Data_Table_Even_Rows)



Mental_Health_Codes <- c("4.1.1",  "4.1.2",  "4.1.3",  "4.2.1",  "4.2.2",  "4.2.3",  
                        "4.3.1",  "4.3.2",  "4.3.3",  "4.3.4",  "4.10.1", "4.10.3")

Antiepileptic_Codes <- c("4.8.1", "4.8.2")


Final_Raw_Data_Table_Even_Rows <- 
  rbind(Final_Raw_Data_Table_Even_Rows, #union old data and new custom groups below
  Final_Raw_Data_Table_Even_Rows %>% #dataset
    filter(ID %in% Mental_Health_Codes) %>% #filter by Mental Health codes set up
    group_by(Date, Year, Month) %>% #group by Date, Year, and Month
    summarise(
      Prescriptions = sum(Prescriptions, na.rm = TRUE), #aggregate prescriptions
      Quantity = sum(Quantity, na.rm = TRUE), #aggregate quantities
      Cost = sum(Cost, na.rm = TRUE), #aggregate costs
      .groups = "drop" #drop groups 
    ) %>% 
    mutate(
      ID = "Custom1", #Make Name ID Custom1 for this group
      Description = "Mental-Health Related Drugs", #Name the description
      BNF_Level = "Custom" #Assign bnf level of custom
    ),
  Final_Raw_Data_Table_Even_Rows %>% #dataset
    filter(ID %in% Antiepileptic_Codes) %>% #filter by Antiepileptic codes set up
    group_by(Date, Year, Month) %>% #group by Date, Year, and Month
    summarise(
      Prescriptions = sum(Prescriptions, na.rm = TRUE), #aggregate prescriptions
      Quantity = sum(Quantity, na.rm = TRUE), #aggregate quantities
      Cost = sum(Cost, na.rm = TRUE), #aggregate costs
      .groups = "drop" #drop groups 
    ) %>% 
    mutate(
      ID = "Custom2", #Make Name ID Custom2 for this group
      Description = "AntiEpileptics", #Name the description
      BNF_Level = "Custom" #Assign bnf level of custom
    )
)    

#Create New Dictionary with custom options based on data we have/custom groupings
BNF_Dictionary <- 
  Final_Raw_Data_Table_Even_Rows %>% #dataset
  select(ID, Description, BNF_Level) %>% #select ID, Description, and BNF Level
  unique() #get unique/distinct rows

# BNF_Dictionary <- 
#   rbind(BNF_Dictionary,
#       c("Custom1", "AntiEpileptics", "Custom"),
#       c("Custom2", "Mental-Health Related Drugs", "Custom")
#     )





Long_Raw_Data_Table <- 
  Final_Raw_Data_Table_Even_Rows %>% #dataset
  pivot_longer( #making 3 columns into 2
    cols = c("Prescriptions", "Quantity", "Cost"), #choose columns want to pivot on
    names_to = "Metric_Name", #columns become string in metric name
    values_to = "Value" #associated numeric value goes here
  )


YoY_Calculations <- function() { #Input list of drug descriptions
  
  NewTable <- data.frame(            #Create empty df (will append rows)
    Description = character(), #Description of Chapter, Section, or Paragraph
    Date=character(),         #date (date in YYYY-MM-DD)
    Quantity=double(),        #quantity dispensed here
    Prescriptions=double(),   #prescription count here
    Cost=double(),            #cost of prescriptions here
    stringsAsFactors=FALSE) #keep strings as strings
  
  
#iterate through all descriptions
for(Description_Current in unique(Final_Raw_Data_Table_Even_Rows$Description)) {
  
YoY_Values_Current <- 
  Final_Raw_Data_Table_Even_Rows %>% #dataset
  filter(Description %in% Description_Current) %>% #filter selected Descriptions
  select(Date, Description, Quantity, Prescriptions, Cost) %>% #select columns needed
  group_by(Description, Date) %>% #groups for calculation (look back 12 months (date for each description))
  summarise( #3 columns we will calculate on
    Quantity = Quantity, 
    Prescriptions = Prescriptions,
    Cost = Cost,
    .groups = 'drop'
  ) %>%
  arrange(Date, .by_group = TRUE) %>% #sort date for lag calculation
  mutate( 
    YoY_Quantity = round(
      (100*Quantity/lag(Quantity,12)) - 100, 
      1),  #look back 12 previous dates on group, then calculate year over year % for quantity  
    YoY_Prescriptions = round(
      (100*Prescriptions/lag(Prescriptions,12)) - 100,
      1),  #look back 12 previous dates on group, then calculate year over year % for prescriptions
    YoY_Cost = round(
      (100*Cost/lag(Cost,12)) - 100,
      1)   #look back 12 previous dates on group, then calculate year over year % for cost
  ) %>%
  select (Description, Date, YoY_Quantity, YoY_Prescriptions, YoY_Cost) %>% #ensure necessary columsn are selected
  rename(
    Quantity = YoY_Quantity, #rename quantity 
    Prescriptions = YoY_Prescriptions, #rename prescriptions
    Cost = YoY_Cost #rename cost
  )

#print(YoY_Values_Current)


NewTable <- rbind(NewTable, YoY_Values_Current) #union values before next iteration 
}


return(NewTable) #return the full table 
}


Long_YoY_Numbers <- 
  left_join (YoY_Calculations(), #run YoY calculations to get all data
                                  BNF_Dictionary, #join to bring in other info-columns
                               by = "Description") %>% #join on Description
  mutate(
  
      #Add year column
      #1. extract year from through format function
      #2. make numeric
      Year = as.numeric( #2
          format(Date, #1
                "%Y") #2
      ),

      #Add month column
      #1. extract month from through format function
      #2. make numeric
      #3  use month.abb to change to 3 letter month abbreviation
     Month = month.abb[  #3
              as.numeric(  #2
                format(Date, #1
                       "%m") #2
              )]
  ) %>%
  select (ID, Description, BNF_Level, Date, Year, Month, Quantity, Prescriptions, Cost) %>% #select necessary colums
  pivot_longer( #making 3 columns into 2
    cols = c("Prescriptions", "Quantity", "Cost"), #choose columns want to pivot on
    names_to = "Metric_Name", #columns become string in metric name
    values_to = "Value" #associated numeric value goes here
  )

#remove table
rm(Final_Raw_Data_Table_Even_Rows)


#function for shiny app to get YoY comparison between two selected drug codes
YoY_Comparison_Long <- function(Group1Name, #drug code 1
                           Group2Name, #drug code 2
                           Metric_For_Evaluation) { #metric selected to evaluate
  
  Group1 <- 
    Long_YoY_Numbers %>% #dataset 
    filter(Description == Group1Name) %>% #filter on description
    filter(Metric_Name == Metric_For_Evaluation) %>% #filter on metric chosen
    select(Description,  Date, Year, Month, Metric_Name, Value) #select necessary columns
  
  Group2 <- 
    Long_YoY_Numbers %>% #dataset 
    filter(Description == Group2Name) %>% #filter on description
    filter(Metric_Name == Metric_For_Evaluation) %>% #filter on metric chosen
    select(Description,  Date, Year, Month, Metric_Name, Value) #select necessary columns
  
  YOY_Comparison_Table <- 
    left_join(Group1, Group2, by = "Date") %>% #join both groups on date
    rename(
      Description.Base = Description.y, #Rename Drug Description 2
      Description.Measured = Description.x,  #Rename Drug Description 1
      Year = Year.x, #Year Column
      Month = Month.x #Month Column
    ) %>%
    mutate(
      Value_Comparison = Value.x - Value.y #Calculate percent difference
    ) %>%
    select(Description.Base, Description.Measured, #select necessary columns
           Year, Month, Value_Comparison)
  
  
}

getwd()
save.image(file = "Open_Prescribing_Shiny.RData")
