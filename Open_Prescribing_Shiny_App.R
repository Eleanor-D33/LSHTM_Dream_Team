########################################################
# Rshiny Open Prescribing Exploratory Data Analysis    #
# 2-Feb-2020                                          #
########################################################
# rm(list=ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

library(shiny)
library(tidyverse)
library(ggplot2)

#assign color for each year for graphs
YearColor <- data.frame(Year = c("2016", "2017","2018","2019","2020"),
                        Color = c("brown", "skyblue", "darkblue", "limegreen", "red")
)

#load("Open_Prescribing_Shiny.RData")


# Define server logic for application
server <- function(input, output) {
  
  #
  output$Drug_Codes_1 <- renderUI({
    #dropdown dynamic input for drug code #1 
    selectInput("Drug_Code_Select_1", #variable name for Drug Code #1 within rest of R code
                "Drug Code #1", #printed text
               #Choices determined based on another input BNF Level, all descriptions remaining shown
                choices = BNF_Dictionary[BNF_Dictionary$BNF_Level==input$BNF_Level,"Description"], 
                            selected = "Antiepileptic drugs", #default section
                selectize=TRUE) #allows search in drop down
  })
  
  output$Drug_Codes_2 <- renderUI({
    
    selectInput("Drug_Code_Select_2", #variable name for Drug Code #2 within rest of R code
                "Drug Code #2", #printed text
                #Choices determined based on another input BNF Level, all descriptions remaining shown
                choices = BNF_Dictionary[BNF_Dictionary$BNF_Level==input$BNF_Level,"Description"],
                            selected = "Drugs used in psychoses and related disorders", #default section
                selectize=TRUE) #allows search in drop down
  })
  
  #Call plots
  
  output$VolumePlot <- renderPlot({
     
    Year_Range <- input$Year_Range #assigns Year Range slider values to Year_Range
    
    Metric_to_Eval <- input$Metric_Evaluated #Assigns Metric_Evaluated drop down to Metric_to_Eval
    
    Drug_Codes_Selected <- c(input$Drug_Code_Select_1, #Assigns two Drug Codes to two item list
                             input$Drug_Code_Select_2)
     
    Region_to_Eval <- input$Region_Input #Assigns Region selected to Region_to_Eval
    
     #Filters out assigned year-color scheme for years to be present in graph
     GraphScale <- YearColor %>% 
       filter(as.numeric(Year) >= Year_Range[1] & Year <=  Year_Range[2])

    
     Long_Raw_Data_Table %>% #pre-processed data table
       filter(Year >= Year_Range[1] & Year <=  Year_Range[2]) %>% # filter for years selected
       filter(Description %in% Drug_Codes_Selected) %>% #filter for Descriptions selected
       filter(Metric_Name == Metric_to_Eval) %>% #filter for metric chosen to evaluate
       filter(Region == Region_to_Eval) %>% #filter for Region chosen to evaluate
       ggplot(aes(x = Month, #month on x-axis
                  y = Value, #value (for metric chosen) as y-axis
                  color = factor(Year), #set year as factor for color
                  #fill = factor(Year),
                  group = factor(Year))) + #group by year as well
       geom_line() + #add lines to graph
       geom_point() + #add points to graph
       facet_grid(rows = vars(Description), #break out graphs into facets by Descriptions
                  scales = "free_y",  # Let the width of facets vary and force all bars to have the same width.
                  switch = "x")  +    # Move the facet labels to the bottom.
       theme(axis.text.x = element_text(size = 10), #x axis aesthetics
             axis.text.y = element_text(hjust = 0.5, size = 10), #y axis aesthetics
             plot.title = element_text(hjust = 0.5, size = 30), #plot title aesthetics
             legend.title = element_text(size = 25), #legend title aesthetics
             legend.text = element_text(size = 15)) + #legend text aesthetics
       labs(title= paste(Metric_to_Eval, "By Drug Class"), #graph title (metric eval pasted in)
            x ="Month", #Name Month as x axis title
            y = Metric_to_Eval, #Name Metric chosen as Y axis title
            color = "Year") + #Color names legend title
       scale_y_continuous(labels = function(x) format(x, scientific = TRUE)) + #y axis labels in scientific format
       scale_x_discrete(limits = month.abb) + #month 3-letter abbreviations 
       scale_color_manual(breaks = GraphScale$Year, #breaks for Years
                         values= GraphScale$Color #colors of graph based on Years
       )
     
    
  }, height = 500, width = 800) # Size of plot
  
  
  output$VolumePlotTable <- renderTable({
    
          Year_Range <- input$Year_Range #assigns Year Range slider values to Year_Range
          
          Metric_to_Eval <- input$Metric_Evaluated #Assigns Metric_Evaluated drop down to Metric_to_Eval
          
          Drug_Codes_Selected <- c(input$Drug_Code_Select_1, #Assigns two Drug Codes to two item list
                                   input$Drug_Code_Select_2)
          
          Region_to_Eval <- input$Region_Input #Assigns Region selected to Region_to_Eval
          
          #Filters out assigned year-color scheme for years to be present in graph
          GraphScale <- YearColor %>% 
            filter(as.numeric(Year) >= Year_Range[1] & Year <=  Year_Range[2])
          
          
          Long_Raw_Data_Table %>% #pre-processed data table
            filter(Year >= Year_Range[1] & Year <=  Year_Range[2]) %>% # filter for years selected
            filter(Description %in% Drug_Codes_Selected) %>% #filter for Descriptions selected
            filter(Metric_Name == Metric_to_Eval) %>% #filter for metric chosen to evaluate
            filter(Region == Region_to_Eval) %>% #filter for Region chosen to evaluate
            select(Description, Year, Month, Value) %>% 
            mutate(Year = as.character(Year),
                   Desc = paste(substr(Description,1,10), "...", sep="")#,
                   #Value = RelabelFunc(Value)
                   ) %>% 
            select(Desc, Year, Month, Value) %>% 
            pivot_wider(names_from = Month,
                         values_from = Value)
  },
  options = list(autoWidth = TRUE)
                
  )
  
  output$YoYPlot <- renderPlot({
    
    Year_Range <- input$Year_Range #assigns Year Range slider values to Year_Range
    
    Metric_to_Eval <- input$Metric_Evaluated #Assigns Metric_Evaluated drop down to Metric_to_Eval
    
    Drug_Codes_Selected <- c(input$Drug_Code_Select_1, #Assigns two Drug Codes to two item list
                             input$Drug_Code_Select_2)
    
    Region_to_Eval <- input$Region_Input #Assigns Region selected to Region_to_Eval
    
    Long_YoY_Numbers %>% 
      filter(Year >= Year_Range[1] & Year <=  Year_Range[2]) %>% # filter for years selected
      filter(Description %in% Drug_Codes_Selected) %>% #filter for Descriptions selected
      filter(Metric_Name == Metric_to_Eval) %>% #filter for metric chosen to evaluate
      filter(Region == Region_to_Eval) %>% #filter for Region chosen to evaluate
      ggplot(aes(x = Month, #month on x-axis
                 y = Value, #value (for metric chosen) as y-axis
                 #color = Description, 
                 fill = Description, #fill color for bars based on description
                 group = Description #groupings based on description
      )) +
      geom_bar(stat = "identity", position = "dodge") + #set bar types for graph
      facet_grid(rows = vars(factor(Year)), #break out graphs into facets by Descriptions
                 scales = "free_x",  # Let the width of facets vary and force all bars to have the same width.
                 switch = "y")  +    # Move the facet labels to the bottom.
      theme(axis.text.x = element_text(size = 10), #x axis aesthetics
            axis.text.y = element_text(hjust = 0.5, size = 10), #y axis aesthetics
            plot.title = element_text(hjust = 0.5, size = 30), #plot title aesthetics
            legend.title = element_text(size = 15), #legend title aesthetics
            legend.text = element_text(size = 15), #legend text aesthetics
            legend.position = "bottom") + #move legend to bottom of graph
      labs(title=paste(Metric_to_Eval, "YoY Change By Drug Class"), #graph title (metric eval pasted in)
           x ="Month", #Name Month as x axis title
           y = "% Change" #Name % change as Y axis title
      ) +
      scale_x_discrete(limits = month.abb) + #month 3-letter abbreviations
      scale_y_continuous(labels = function(x) paste(x,"%",sep ="")) + #add percent sign to all x-axis labels
      scale_fill_manual(breaks = Drug_Codes_Selected, #breaks by 2 drug codes selected
                        values=c("purple", "orange") #values purple for 1st, orange for 2nd drug code selected
      )
    
    
  }, height = 500, width = 800) #size of graph
  
  output$YoYPlotTable <- renderTable({
    
    Year_Range <- input$Year_Range #assigns Year Range slider values to Year_Range
    
    Metric_to_Eval <- input$Metric_Evaluated #Assigns Metric_Evaluated drop down to Metric_to_Eval
    
    Drug_Codes_Selected <- c(input$Drug_Code_Select_1, #Assigns two Drug Codes to two item list
                             input$Drug_Code_Select_2)
    
    Region_to_Eval <- input$Region_Input #Assigns Region selected to Region_to_Eval
    
    #Filters out assigned year-color scheme for years to be present in graph
    GraphScale <- YearColor %>% 
      filter(as.numeric(Year) >= Year_Range[1] & Year <=  Year_Range[2])
      
      Long_YoY_Numbers %>% #pre-processed data table
      filter(Year >= Year_Range[1] & Year <=  Year_Range[2]) %>% # filter for years selected
      filter(Description %in% Drug_Codes_Selected) %>% #filter for Descriptions selected
      filter(Metric_Name == Metric_to_Eval) %>% #filter for metric chosen to evaluate
      filter(Region == Region_to_Eval) %>% #filter for Region chosen to evaluate
      select(Description, Year, Month, Value) %>% 
      mutate(Year = as.character(Year),
             Desc = paste(substr(Description,1,10), "...", sep=""),
             Value = paste(round(Value,1), "%", sep ="")
      ) %>% 
      select(Desc, Year, Month, Value) %>% 
      pivot_wider(names_from = Month,
                  values_from = Value)
  },
  options = list(autoWidth = TRUE)
  
  )
  
  
  
  output$YoYCompare <- renderPlot({
    
    Year_Range <- input$Year_Range #assigns Year Range slider values to Year_Range
    
    Metric_to_Eval <- input$Metric_Evaluated #Assigns Metric_Evaluated drop down to Metric_to_Eval
    
    Drug_Codes_Selected <- c(input$Drug_Code_Select_1, #Assigns two Drug Codes to two item list
                             input$Drug_Code_Select_2)
    
    Region_to_Eval <- input$Region_Input #Assigns Region selected to Region_to_Eval
    
    #Filters out assigned year-color scheme for years to be present in graph
    GraphScale <- YearColor %>% 
      filter(as.numeric(Year) >= Year_Range[1] & Year <=  Year_Range[2])
    
    
    YOY_Comparison_Table <- #This is a created function to calculate the difference in YoY % Change for
      YoY_Comparison_Long(Drug_Codes_Selected[1], #Drug Code 1
                          Drug_Codes_Selected[2], #by Drug Code 2
                          Metric_to_Eval, # for the chosen metric
                          Region_to_Eval) #filter for Region chosen to evaluate
    
    YOY_Comparison_Table %>% 
      filter(Year >= Year_Range[1] & Year <=  Year_Range[2]) %>% # filter for years selected
      mutate (Year = factor(Year)) %>% #Change year to factor
      ggplot(aes(x = Month, #month on x-axis
                 y = Value_Comparison, #Value Compared on y-axis (based on metric chosen)
                 #color = Year, 
                 fill = Year #fill color for bars based on Year
                 #group = factor(Year)
      )) +
      geom_bar(stat = "identity", position = "dodge") + #set bar types for graph
      scale_x_discrete(limits = month.abb) + #month 3-letter abbreviations
      scale_y_continuous(labels = function(x) paste(x,"%",sep ="")) + #add percent sign to all x-axis labels
      theme(axis.text.x = element_text(size = 10), #x axis aesthetics
            axis.text.y = element_text(hjust = 0.5, size = 10), #y axis aesthetics
            plot.title = element_text(hjust = 0.5, size = 15), #plot title aesthetics
            legend.title = element_text(size = 25), #legend title aesthetics
            legend.text = element_text(size = 15)) + #legend text aesthetics
      labs(title=paste("YoY Cost % Difference Between \n", #graph title (with line break)
                       Drug_Codes_Selected[1], "&", #paste Drug Code 1
                       Drug_Codes_Selected[2]), #paste Drug Code 1
           x ="Month", #Name Month as x axis title
           y = "Percent Difference"#Name Percent Difference as y axis title
      ) +
      scale_fill_manual(breaks = GraphScale$Year, #breaks for Years
                         values= GraphScale$Color #set colors for graph based on years
      )
   
   }, height = 500, width = 800) #size of graph
  
  output$YoYCompareTable <- renderTable({
    
    Year_Range <- input$Year_Range #assigns Year Range slider values to Year_Range
    
    Metric_to_Eval <- input$Metric_Evaluated #Assigns Metric_Evaluated drop down to Metric_to_Eval
    
    Drug_Codes_Selected <- c(input$Drug_Code_Select_1, #Assigns two Drug Codes to two item list
                             input$Drug_Code_Select_2)
    
    Region_to_Eval <- input$Region_Input #Assigns Region selected to Region_to_Eval
    
    #Filters out assigned year-color scheme for years to be present in graph
    GraphScale <- YearColor %>% 
      filter(as.numeric(Year) >= Year_Range[1] & Year <=  Year_Range[2])
    
    YOY_Comparison_Table <- #This is a created function to calculate the difference in YoY % Change for
      YoY_Comparison_Long(Drug_Codes_Selected[1], #Drug Code 1
                          Drug_Codes_Selected[2], #by Drug Code 2
                          Metric_to_Eval, # for the chosen metric
                          Region_to_Eval) #filter for Region chosen to evaluate
    
    YOY_Comparison_Table %>% 
      filter(Year >= Year_Range[1] & Year <=  Year_Range[2]) %>% # filter for years selected
      mutate (Year = factor(Year)) %>% #Change year to factor
      select(Year, Month, Value_Comparison) %>% 
      mutate(Year = as.character(Year),
             Value_Comparison = paste(round(Value_Comparison,1), "%", sep ="")
      ) %>% 
      select(Year, Month, Value_Comparison) %>% 
      pivot_wider(names_from = Month,
                  values_from = Value_Comparison)
  },
  options = list(autoWidth = TRUE)
  
  )
   
  
  output$PercentofRegionComparison <- renderPlot({
    
    Year_Range <- input$Year_Range #assigns Year Range slider values to Year_Range
    
    Metric_to_Eval <- input$Metric_Evaluated #Assigns Metric_Evaluated drop down to Metric_to_Eval
    
    Drug_Codes_Selected <- c(input$Drug_Code_Select_1, #Assigns two Drug Codes to two item list
                             input$Drug_Code_Select_2)
    
    Region_to_Eval <- input$Region_Input #Assigns Region selected to Region_to_Eval
    
    #Filters out assigned year-color scheme for years to be present in graph
    GraphScale <- YearColor %>% 
      filter(as.numeric(Year) >= Year_Range[1] & Year <=  Year_Range[2])
    
    Percent_of_Region <- #This is a created function to calculate the percent of a drug class for a region
      Total_Avg_Comparison(Drug_Codes_Selected[1], #Drug Code 1
                           Drug_Codes_Selected[2], #by Drug Code 2
                           Metric_to_Eval, # for the chosen metric
                           Region_to_Eval) #filter for Region chosen to evaluate
  
    
    Percent_of_Region %>% 
      filter(Year >= Year_Range[1] & Year <=  Year_Range[2]) %>% # filter for years selected
      mutate (Year = factor(Year)) %>% #Change year to factor
      ggplot(aes(x = Month, #month on x-axis
                 y = Value_Comparison, #Value Compared on y-axis (based on metric chosen)
                 color = Year, 
                 #fill = Year #fill color for bars based on Year
                 group = Year
      )) +
      geom_line() + #add lines to graph
      geom_point() + #add points to graph
      scale_x_discrete(limits = month.abb) + #month 3-letter abbreviations
      scale_y_continuous(labels = function(x) paste(x,"%",sep ="")) + #add percent sign to all x-axis labels
      facet_grid(rows = vars(Description), #break out graphs into facets by Descriptions
                 scales = "free_y",  # Let the width of facets vary and force all bars to have the same width.
                 switch = "y")  +    # Move the facet labels to the bottom.
      theme(axis.text.x = element_text(size = 10), #x axis aesthetics
            axis.text.y = element_text(hjust = 0.5, size = 10), #y axis aesthetics
            plot.title = element_text(hjust = 0.5, size = 15), #plot title aesthetics
            legend.title = element_text(size = 25), #legend title aesthetics
            legend.text = element_text(size = 15)) + #legend text aesthetics
      labs(title=paste("Percent of Region \n", #graph title (with line break)
                       Drug_Codes_Selected[1], "&", #paste Drug Code 1
                       Drug_Codes_Selected[2]), #paste Drug Code 1
           x ="Month", #Name Month as x axis title
           y = "Percent of Region"#Name Percent Difference as y axis title
      ) +
      scale_color_manual(breaks = GraphScale$Year, #breaks for Years
                        values= GraphScale$Color #set colors for graph based on years
      )
    
    
    
    
    
    
  }, height = 500, width = 800) #size of graph
  
  output$PercentofRegionComparisonTable <- renderTable({
    
    Year_Range <- input$Year_Range #assigns Year Range slider values to Year_Range
    
    Metric_to_Eval <- input$Metric_Evaluated #Assigns Metric_Evaluated drop down to Metric_to_Eval
    
    Drug_Codes_Selected <- c(input$Drug_Code_Select_1, #Assigns two Drug Codes to two item list
                             input$Drug_Code_Select_2)
    
    Region_to_Eval <- input$Region_Input #Assigns Region selected to Region_to_Eval
    
    #Filters out assigned year-color scheme for years to be present in graph
    GraphScale <- YearColor %>% 
      filter(as.numeric(Year) >= Year_Range[1] & Year <=  Year_Range[2])
    
    Percent_of_Region <- #This is a created function to calculate the percent of a drug class for a region
      Total_Avg_Comparison(Drug_Codes_Selected[1], #Drug Code 1
                           Drug_Codes_Selected[2], #by Drug Code 2
                           Metric_to_Eval, # for the chosen metric
                           Region_to_Eval) #filter for Region chosen to evaluate
    
    
    Percent_of_Region %>% 
      filter(Year >= Year_Range[1] & Year <=  Year_Range[2]) %>% # filter for years selected
      mutate (Year = factor(Year)) %>% #Change year to factor
      select(Description, Year, Month, Value_Comparison) %>% 
      mutate(Year = as.character(Year),
             Desc = paste(substr(Description,1,10), "...", sep=""),
             Value_Comparison = paste(round(Value_Comparison,2), "%", sep ="")
      ) %>% 
      select(Desc, Year, Month, Value_Comparison) %>% 
      pivot_wider(names_from = Month,
                  values_from = Value_Comparison)
  },
  options = list(autoWidth = TRUE)
  
  )
  
}

##### UI #####


ui <- shinyUI(fluidPage(
  
  titlePanel("Prescription Data Analysis"), #Title panel of page
  
  sidebarLayout(
    sidebarPanel(
      
      p('User will select which level to look through descriptions'), #outputted text
      p('(We can add a custom option for our own groupings too)'), #outputted text
      
      
      selectInput("Region_Input", #variable name for BNF_Level within rest of R code
                  "Select Region", #printed text
                  c("England",
                    "East of England",
                    "London",
                    "Midlands",
                    "North East and Yorkshire",
                    "North West",              
                    "South East",              
                    "South West",              
                    "Unknown"),
                  selected = c("England")), #default value
      
      
      selectInput("BNF_Level", #variable name for BNF_Level within rest of R code
                  "Selection Level", #printed text
                  c("Custom" = "Custom", #"Printed Option" matched with "Relevant table value"
                    "Chapter" = "BNF chapter", 
                    "Section" = "BNF section", 
                    "Paragraph" = "BNF paragraph"),
                  selected = c("Section"= "BNF section")), #default value
      
      p('Select Drug Code 1'), #outputted text
      
      uiOutput("Drug_Codes_1"), #user interface placement of dynamic selectInput for Drug_Codes_1

      p('Select Drug Code 2'), #outputted text
      
      uiOutput("Drug_Codes_2"), #user interface placement of dynamic selectInput for Drug_Codes_2
      
      selectInput("Metric_Evaluated", #variable name for Metric_Evaluated within rest of R code
                  "Metric", #printed text
                  c("Prescriptions", #selection options
                    "Cost"
                    )),
      
      sliderInput('Year_Range', #Variable name for Year_Range within rest of R code
                  'Years Range', #printed text
                  min=2017, max=2020, #range of slider
                  value=c(2018, 2020), #default values
                  step=1) # 1 point gap between selections in slider
      
      
    ),
    
    mainPanel(
      # p('User will select different tabs to see different graphs'), #outputted text
      # p('2016 has no YoY data'), #outputted text
      # hr(),
      tabsetPanel(
        tabPanel("Volume Plot", 
                 fixedRow(plotOutput("VolumePlot",width="100%"), div(style = "height:150px")),
                 fixedRow(tableOutput("VolumePlotTable"))
                 ),#tab1 Volume Plot
        tabPanel("YoY By Drug Class", 
                 fixedRow(plotOutput("YoYPlot",width="100%"), div(style = "height:150px")),
                 fixedRow(tableOutput("YoYPlotTable"))
                 ), #tab2 YoY Plot
        tabPanel("YoY Comparing Drug Classes",
                 fixedRow(plotOutput("YoYCompare",width="100%"), div(style = "height:150px")),
                 fixedRow(tableOutput("YoYCompareTable"))         
                 
                 ), #tab3 YoY compare plot
        tabPanel("Percent of Region Comparison", 
                 plotOutput("PercentofRegionComparison", width="100%"), div(style = "height:150px"),
                fixedRow(tableOutput("PercentofRegionComparisonTable")) #tab4 Percent of Region compare plot
                )
      )
    )
  )
))  


##### Run #####
shinyApp(ui = ui, server = server)
