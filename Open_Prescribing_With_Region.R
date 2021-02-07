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

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

Region_CSV <- read.csv("Mapping_For_Regions.csv")



###################################################
## Function to extract data from OpenPrescribing ##
###################################################
##-----------------------------------------------------------------
Get_Spending_By_CCG <- function(drug_codes) { #function input is a single drug code
  #can be BNF Chapter, Section, or Paragraph
  
  
  NewTable <- data.frame(            #Create empty df (will append rows)
    id=character(),        #id (BNF Chapter, Section, or Paragraph)
    Region=character(),    #Region based on CCG
    actual_cost=double(),  #actual_cost (dollars spent)
    date=character(),      #date (month in YYYY-MM-DD)
    items=integer(),       #items (prescription volume)
    quantity=integer(),    #quantity (quantity dispensed)
    stringsAsFactors=FALSE) #keep strings as strings
  
  #iterate for all ids
  for (val in drug_codes$id) {      
    
    #some ids have no data, so catch errors for empty codes  
    ID_Attempt <- 
      try({temp <- spending_by_CCG(chemical_section_or_presentation_code = val)}, #use openPrescribingR function to pull data
          silent = TRUE) #keep executing code if error caught
    
    #if error was caught above, we do not want to append the data to NewTable
    if (class(ID_Attempt) == "try-error") { 
      print(paste(val, "has no data")) #print out message that id has no data
    }
    else{
      print(paste(val, "has data")) #print out message that id has no data
      temp$id <- val #add id to temp df (the one we currently our iterating through)
      temp$row_id <- as.character(temp$row_id)
      
      
      temp <- left_join(temp, Region_CSV, by = "row_id") 
      
      # Missing_Values <- 
      #   rbind(Missing_Values,
      #         temp[is.na(temp$Region)] %>% 
      #         select(row_id,
      #                row_name) %>% 
      #         unique()
      # )
      
      #print(temp$Region[is.na(temp$Region)])
              
      temp$Region[is.na(temp$Region)] <- "Unknown2"
      
      temp <-
        temp %>%
        select(id, Region, date, items, quantity, actual_cost) %>%
        group_by(id, Region, date) %>%
        summarise(
          items = sum(items, na.rm = TRUE),
          quantity = sum(quantity, na.rm = TRUE),
          actual_cost = sum(actual_cost, na.rm = TRUE),
          .groups = 'drop'
        )
      
      
      
      NewTable <- rbind(NewTable,temp) #append to our NewTable df
    }
  }
  
  
  #Set var date to date type using YYYY-mm-dd format
  NewTable$date <- as.Date(NewTable$date, format="%Y-%m-%d")
  
  #Rename Date Column
  NewTable <- 
    NewTable %>%
    rename(
      Date = date
    )
  
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
  
  
  # NewTable <- 
  #   left_join(NewTable, Region_CSV, by = "row_id") %>%
  #   select(id, name, type, Region, Date, year, month, items, quantity, actual_cost) 
  # 
  
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
  filter(substr(id,1,2) != "18") %>% 
  filter(substr(id,1,2) != "20") %>% 
  filter(substr(id,1,2) != "21") %>% 
  filter(substr(id,1,2) != "22") %>% 
  filter(substr(id,1,2) != "23") %>% 
  unique      #ensures no duplication of codes/name/type 

rm(Drug_Detail_Table)
  
  
All_Raw_Data_Pull <- Get_Spending_By_CCG(BNF_Dictionary) #takes over 10 mins 

#rm(temp) <- Final_Raw_Data_Table %>% select(Region) %>% unique()
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
  as.data.frame(
    Final_Raw_Data_Table %>%
    select(id, name, type, Region, Date, year, month, items, quantity, actual_cost) %>% #removes date col
    rename(
      Prescriptions = items, #total prescription volume 
      Cost = actual_cost, #total dollars spent
      Quantity = quantity, #total quantity dispensed 
      ID = id, #id (BNF Chapter, Section, or Paragraph)
      Description = name, #description for BNF code
      BNF_Level = type, #Most granular level of BNF code
      Year = year, #year
      Month = month #montH
    )
  )

###ENSURING WE HAVE CORRECT # OF ROWS FOR ANALYSIS WITH LAG FUNCTION

#Get all unique descriptions
All_Descriptions <- Final_Raw_Data_Table %>%
  select(Description, ID, BNF_Level) %>%
  unique()

#Get all unique dates
All_Dates <- Final_Raw_Data_Table %>%
  select(Date, Year, Month) %>%
  unique()

#Get all unique CCG's
All_Regions <- Final_Raw_Data_Table %>%
  select(Region) %>%
  unique()


#cartesian joins to get all date/description/CCG combos
All_Combinations <- 
  merge(All_Regions, 
        merge(All_Descriptions,All_Dates, all=TRUE),
        all=TRUE
  )

#join the date/description combos back to the data
Final_Raw_Data_Table_Even_Rows <- 
  left_join(All_Combinations, Final_Raw_Data_Table, by= c("ID", "Date", "Region")) %>%
  select(Region, Description.x, ID, BNF_Level.x, Date, Prescriptions, Quantity, Cost) %>% 
  rename(
    Description = Description.x,
    BNF_Level = BNF_Level.x,
  ) %>%
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
        )],
    
    Prescriptions = replace_na(Prescriptions, 0),
    
    Quantity = replace_na(Quantity, 0),
    
    Cost = replace_na(Cost, 0)
    
  ) %>% 
  select(Region, Description, ID, BNF_Level, Date, Year, Month, Prescriptions, Quantity, Cost)

  
  
  
#remove table
rm(Final_Raw_Data_Table)



Mental_Health_Codes <- c("4.1.1",  "4.1.2",  "4.1.3",  "4.2.1",  "4.2.2",  "4.2.3",  
                         "4.3.1",  "4.3.2",  "4.3.3",  "4.3.4",  "4.10.1", "4.10.3")

Antiepileptic_Codes <- c("4.8.1", "4.8.2")


Final_Raw_Data_Table_Even_Rows <- 
  rbind(Final_Raw_Data_Table_Even_Rows, #union old data and new custom groups below
        Final_Raw_Data_Table_Even_Rows %>% #dataset
          filter(ID %in% Mental_Health_Codes) %>% #filter by Mental Health codes set up
          group_by(Date, Year, Month, Region) %>% #group by Date, Year, and Month
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
          group_by(Date, Year, Month, Region) %>% #group by Date, Year, and Month
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


Final_Raw_Data_Table_Even_Rows <- rbind(
  Final_Raw_Data_Table_Even_Rows,
  Final_Raw_Data_Table_Even_Rows %>% #dataset
    group_by(Date, Year, Month, BNF_Level, Description, ID) %>% #group by Date, Year, and Month
    summarise(
      Prescriptions = sum(Prescriptions, na.rm = TRUE), #aggregate prescriptions
      Quantity = sum(Quantity, na.rm = TRUE), #aggregate quantities
      Cost = sum(Cost, na.rm = TRUE), #aggregate costs
      .groups = "drop" #drop groups 
    ) %>% 
    mutate (
      Region = "England"
    )
)


#Create New Dictionary with custom options based on data we have/custom groupings
BNF_Dictionary <- 
  Final_Raw_Data_Table_Even_Rows %>% #dataset
  select(ID, Description, BNF_Level) %>% #select ID, Description, and BNF Level
  unique() #get unique/distinct rows



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
    Region=character(),       #Region of Prescriptions
    Quantity=double(),        #quantity dispensed here
    Prescriptions=double(),   #prescription count here
    Cost=double(),            #cost of prescriptions here
    stringsAsFactors=FALSE) #keep strings as strings
  
  
  #iterate through all descriptions
  for(Description_Current in unique(Final_Raw_Data_Table_Even_Rows$Description)) {
    
    for(Region_Current in unique(Final_Raw_Data_Table_Even_Rows$Region)) {
      
      
      YoY_Values_Current <- 
        Final_Raw_Data_Table_Even_Rows %>% #dataset
        filter(Description %in% Description_Current) %>% #filter selected Descriptions
        filter(Region %in% Region_Current) %>% #filter selected Descriptions
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
      
      YoY_Values_Current$Region = Region_Current
      
      #print(YoY_Values_Current)
      
      
      NewTable <- rbind(NewTable, YoY_Values_Current) #union values before next iteration 
    }
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
  select (ID, Description, BNF_Level, Region, Date, Year, Month, Quantity, Prescriptions, Cost) %>% #select necessary colums
  pivot_longer( #making 3 columns into 2
    cols = c("Prescriptions", "Quantity", "Cost"), #choose columns want to pivot on
    names_to = "Metric_Name", #columns become string in metric name
    values_to = "Value" #associated numeric value goes here
  ) %>% 
  filter(Metric_Name != "Quantity") #Remove Quantity from Metrics

Long_Raw_Data_Table <- Long_Raw_Data_Table %>% 
  filter(Metric_Name != "Quantity") #Remove Quantity from Metrics


#remove table
rm(Final_Raw_Data_Table_Even_Rows)


#function for shiny app to get YoY comparison between two selected drug codes
YoY_Comparison_Long <- function(Group1Name, #drug code 1
                                Group2Name, #drug code 2
                                Metric_For_Evaluation,#metric selected to evaluate
                                Region_For_Evaluation) { 
  
  Group1 <- 
    Long_YoY_Numbers %>% #dataset 
    filter(Description == Group1Name) %>% #filter on description
    filter(Metric_Name == Metric_For_Evaluation) %>% #filter on metric chosen
    filter(Region == Region_For_Evaluation) %>% #filter on region chosen
    select(Description, Region,  Date, Year, Month, Metric_Name, Value) #select necessary columns
  
  Group2 <- 
    Long_YoY_Numbers %>% #dataset 
    filter(Description == Group2Name) %>% #filter on description
    filter(Metric_Name == Metric_For_Evaluation) %>% #filter on metric chosen
    filter(Region == Region_For_Evaluation) %>% #filter on region chosen
    select(Description, Region, Date, Year, Month, Metric_Name, Value) #select necessary columns
  
  YOY_Comparison_Table <- 
    left_join(Group1, Group2, by = "Date") %>% #join both groups on date
    rename(
      Description.Base = Description.y, #Rename Drug Description 2
      Description.Measured = Description.x,  #Rename Drug Description 1
      Region = Region.x, #Region Column
      Year = Year.x, #Year Column
      Month = Month.x #Month Column
    ) %>%
    mutate(
      Value_Comparison = Value.x - Value.y #Calculate percent difference
    ) %>%
    select(Description.Base, Description.Measured, #select necessary columns
           Region, Year, Month, Value_Comparison)
  
  
}

# temp <- Total_Avg_Comparison("Antiepileptic drugs", 
#                      "Antidepressant drugs", 
#                      "Prescriptions",
#                      "England")
# rm(Group1Name, Group2Name, Metric_For_Evaluation, Region_For_Evaluation)

Total_Avg_Comparison <- function(Group1Name, #drug code 1
                                Group2Name, #drug code 2
                                Metric_For_Evaluation,#metric selected to evaluate
                                Region_For_Evaluation) { 
  
  Total_For_Region <-
    Long_Raw_Data_Table %>% #dataset 
    filter(Metric_Name == Metric_For_Evaluation) %>% #filter on metric chosen
    filter(Region == Region_For_Evaluation) %>% #filter on region chosen
    select(Description, Region,  Date, Year, Month, Metric_Name, Value) %>% #select necessary columns
    group_by(Region,  Date, Year, Month, Metric_Name) %>% 
    summarise(
      Value = sum(Value, na.rm = TRUE),
      .groups = 'drop'
    ) %>% 
    select(Region,  Date, Year, Month, Metric_Name, Value)
  
  Group1 <- 
    Long_Raw_Data_Table %>% #dataset 
    filter(Description == Group1Name) %>% #filter on description
    filter(Metric_Name == Metric_For_Evaluation) %>% #filter on metric chosen
    filter(Region == Region_For_Evaluation) %>% #filter on region chosen
    select(Description, Region,  Date, Year, Month, Metric_Name, Value) #select necessary columns
  
  Group2 <- 
    Long_Raw_Data_Table %>% #dataset 
    filter(Description == Group2Name) %>% #filter on description
    filter(Metric_Name == Metric_For_Evaluation) %>% #filter on metric chosen
    filter(Region == Region_For_Evaluation) %>% #filter on region chosen
    select(Description, Region, Date, Year, Month, Metric_Name, Value) #select necessary columns
  
  Group1_Percents <- 
    left_join(Total_For_Region, Group1, by = c("Date", "Year", "Month", "Metric_Name", "Region")) %>% #join both groups on date
    mutate(
      Value_Comparison = (Value.y/Value.x)*100 #Calculate percent of region total
    ) %>%
    select(Description, Region, Year, Month, Value_Comparison)
  
  Group2_Percents <- 
    left_join(Total_For_Region, Group2, by = c("Date", "Year", "Month", "Metric_Name", "Region")) %>% #join both groups on date
    mutate(
      Value_Comparison = (Value.y/Value.x)*100 #Calculate percent of region total
    ) %>%
    select(Description, Region, Year, Month, Value_Comparison)
  
  return(rbind(Group1_Percents, Group2_Percents))
  
  
}





Long_Raw_Data_Table <- 
  Long_Raw_Data_Table %>% 
  filter(Year >= 2017)

Long_YoY_Numbers <-
  Long_YoY_Numbers %>% 
  filter(Year >= 2017)

getwd()
save.image(file = "Open_Prescribing_Shiny_W_Region.RData")


