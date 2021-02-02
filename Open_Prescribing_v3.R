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
    Items = items, #total prescription volume 
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


RelabelFunc <- function(x) {
  return(paste(as.character(x/(10^6)), "M", sep = ""))
}


Long_Raw_Data_Table <- Final_Raw_Data_Table %>%
  pivot_longer(
    cols = c("Items", "Quantity", "Cost"),
    names_to = "Metric_Name",
    values_to = "Value"
  )
  #filter(Year >= Year_Range[1] & Year <=  Year_Range[2]) %>%
  #filter(ID %in% Graph_IDs) %>%


Graph_IDs <- c("4.8", "4.2")
Metric_to_Eval <- "Items"
#Deviation from the average 


Long_Raw_Data_Table %>%
  #filter(Year >= 2018) %>%
  filter(ID %in% Graph_IDs) %>%
  filter(Metric_Name == Metric_to_Eval) %>%
  ggplot(aes(x = Month, 
             y = Value, 
             color = factor(Year), 
             #fill = factor(year),
             group = factor(Year))) +
  geom_line() +
  geom_point() +
  facet_grid(rows = vars(Description),
             scales = "free_y",  # Let the width of facets vary and force all bars to have the same width.
             switch = "x")  +    # Move the facet labels to the bottom.
  theme(axis.text.x = element_text(angle = 90),
        plot.title = element_text(hjust = 0.5)) +
  labs(title= paste("Monthly", Metric_to_Eval, "By Drug Class By Year"),
       x ="Month", 
       y = Metric_to_Eval,
       color = "Year") +
  scale_y_continuous(labels=RelabelFunc) +
  scale_x_discrete(limits = month.abb)





Final_Raw_Data_Table %>%
  filter(Year >= 2018) %>%
  filter(ID %in% Graph_IDs) %>%
  select(Description, Year, Month, Total_Quantity) %>%
  pivot_wider(names_from = Month, values_from = Total_Quantity) %>%
  select(Description, Year, Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov, Dec)



YoYQuantity <- Final_Raw_Data_Table %>%
  filter(ID %in% Graph_IDs) %>%
  select(Date, Description, Quantity) %>%
  group_by(Description, Date) %>%
  summarise(
    Quantity = Quantity
  ) %>%
  arrange(Date, .by_group = TRUE) %>%
  mutate(
    YoY_Quantity = round(
      (100*Quantity/lag(Quantity,12)) - 100,
      1)
  ) 

YoYItems <- Final_Raw_Data_Table %>%
  filter(ID %in% Graph_IDs) %>%
  select(Date, Description, Items) %>%
  group_by(Description, Date) %>%
  summarise(
    Items = Items
  ) %>%
  arrange(Date, .by_group = TRUE) %>%
  mutate(
    YoY_Items = round(
      (100*Items/lag(Items,12)) - 100,
      1)
  ) 

YoYCost <- Final_Raw_Data_Table %>%
  filter(ID %in% Graph_IDs) %>%
  select(Date, Description, Cost) %>%
  group_by(Description, Date) %>%
  summarise(
    Cost = Cost
  ) %>%
  arrange(Date, .by_group = TRUE) %>%
  mutate(
    YoY_Cost = round(
      (100*Cost/lag(Cost,12)) - 100,
      1)
  ) 


BNF_Dictionary_Temp <- BNF_Dictionary %>%
  rename(
    Description = name
  )

YoYNumbers <- 
  left_join (
    left_join(
      left_join(YoYQuantity, 
                YoYItems, 
                by = c("Description", "Date")),
    YoYCost, by = c("Description", "Date")),
    BNF_Dictionary_Temp, by = "Description")
    

rm(YoYCost,YoYQuantity,YoYItems, BNF_Dictionary_Temp)    

YoYNumbers

#Add year column
#1. extract year from through format function
#2. make numeric 
YoYNumbers$Year <- as.numeric( #2
  format( YoYNumbers$Date, #1
          "%Y") #2
)


#Add month column
#1. extract month from through format function
#2. make numeric
#3  use month.abb to change to 3 letter month abbreviation
YoYNumbers$Month <- 
  month.abb[  #3
    as.numeric(  #2
      format(YoYNumbers$Date, #1
             "%m") #2
    )]


Long_YoY_Numbers <- YoYNumbers %>%
  select (id, Description, Date, Year, Month, YoY_Items, YoY_Quantity, YoY_Cost) %>%
  rename(
    ID = id,
    Items = YoY_Items,
    Quantity = YoY_Quantity,
    Cost = YoY_Cost
  ) %>%
  pivot_longer(
    cols = c("Items", "Quantity", "Cost"),
    names_to = "Metric_Name",
    values_to = "Value"
  )



Long_YoY_Numbers %>% 
  filter(Year >= 2018 & Year <=  2020) %>%
  filter(ID %in% Graph_IDs) %>%
  filter(Metric_Name == "Cost") %>%#Metric_to_Eval) %>%
  ggplot(aes(x = Month, 
             y = Value, 
             color = Description, 
             fill = Description,
             group = Description
  )) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_grid(rows = vars(factor(Year)),
             scales = "free_x",  # Let the width of facets vary and force all bars to have the same width.
             switch = "y")  +    # Move the facet labels to the bottom.
  theme(axis.text.x = element_text(angle = 90),
        plot.title = element_text(hjust = 0.5),
        legend.position = "bottom") +
  labs(title="YoY Quantity Dispensed By Drug Class",
       x ="Month", 
       y = "% Change"
  ) +
  scale_x_discrete(limits = month.abb)


Group1 <- YoYNumbers %>% 
  filter(Description == "Antiepileptic drugs") %>%
  select(Description,  Date, Year, Month, YoY_Quantity, YoY_Items, YoY_Cost)
Group2 <- YoYNumbers %>% 
  filter(Description == "Drugs used in psychoses and related disorders") %>%
  select(Description, Date, Year, Month, YoY_Quantity, YoY_Items, YoY_Cost)

YOY_Comparison_Table <- 
  left_join(Group1, Group2, by = "Date") %>%
  rename(
    Description.Base = Description.y,
    Description.Measured = Description.x,
    Year = Year.x,
    Month = Month.x
  ) %>%
  mutate(
    Cost_Comparison = YoY_Cost.x - YoY_Cost.y,
    Item_Comparison = YoY_Items.x - YoY_Items.y,
    Quantity_Comparison = YoY_Quantity.x - YoY_Quantity.y,
  ) %>%
  select(Description.Base, Description.Measured, 
         Year, Month, YoY_Quantity.x, YoY_Quantity.y,
         Cost_Comparison, Item_Comparison, Quantity_Comparison)

#add in ID
YOY_Comparison_Long <- YOY_Comparison_Table %>%
  select (Description.Base, Description.Measured, Year, Month,
          Item_Comparison, Quantity_Comparison, Cost_Comparison) %>%
  rename(
    Items = Item_Comparison,
    Quantity = Quantity_Comparison,
    Cost = Cost_Comparison
  ) %>%
  pivot_longer(
    cols = c("Items", "Quantity", "Cost"),
    names_to = "Metric_Name",
    values_to = "Value"
  )

  

#Graph for YOY Comparison Between Drug Clases
YOY_Comparison_Long %>% 
  filter(Year >= 2018 & Year <=  2020) %>%
  filter(Metric_Name == "Cost") %>%#Metric_to_Eval) %>%
  ggplot(aes(x = Month, 
             y = Value,
             color = factor(Year), 
             fill = factor(Year)
             #group = factor(Year)
  )) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_x_discrete(limits = month.abb) +
  theme(axis.text.x = element_text(angle = 90),
        plot.title = element_text(hjust = 0.5)) +
  labs(title="YoY Cost % Difference Between Drug Classes",
       x ="Month", 
       y = "Percent Difference"
  ) 

