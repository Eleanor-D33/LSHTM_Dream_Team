#https://geoportal.statistics.gov.uk/datasets/clinical-commissioning-group-to-stp-and-nhs-england-region-april-2020-lookup-in-england/data
CCGs <- read.csv("Clinical_Commissioning_Group_to_STP_and_NHS_England__Region___April_2020__Lookup_in_England.csv")  

CCGs <- CCGs  %>% 
  select(CCG20CDH, CCG20NM, NHSER20NM)

temp <- left_join(Regions_In_Data, CCGs, by = c("row_id" = "CCG20CDH"))

Region_CSV <- temp %>%
  select (row_id, row_name, NHSER20NM) %>%
  rename (
    Region = NHSER20NM
  )

temp <- left_join(spending_by_CCG(chemical_section_or_presentation_code = "2.5.8")
                  %>% mutate(row_id = as.character(row_id)), Region_CSV, by = "row_id") %>% 
  select(row_id, row_name.x, Region) %>% unique()


ValueChanging<- temp

ValueChanging$X = 141
names(Region_CSV)

ValueChanging <- ValueChanging %>% 
  select(X, row_id, row_name.x, Region) %>%
  rename(row_name = row_name.x)

Region_CSV <- Region_CSV %>% select(X,row_id,row_name,Region)

Temp_Region <- Region_CSV

Region_CSV2 <- rbind(Region_CSV,
                     ValueChanging)

Region_CSV <- Region_CSV2


#Region_CSV$Region[is.na(Region_CSV$Region)] <- "Unknown"

organisation_codes()

write.csv(Region_CSV, "Mapping_For_Regions.csv")

Region_CSV <- read.csv("Mapping_For_Regions.csv")
