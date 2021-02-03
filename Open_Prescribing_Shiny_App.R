########################################################
# Rshiny Open Prescribing Exploratory Data Analysis    #
# 2-Feb-2020                                          #
########################################################

library(shiny)
library(tidyverse)
library(ggplot2)

YearColor <- data.frame(Year = c("2016", "2017","2018","2019","2020"),
                        Color = c("brown", "skyblue", "darkblue", "limegreen", "red")
)

#load("Open_Prescribing_Shiny.RData")


# Define server logic for application
server <- function(input, output) {
  
  #
  output$Drug_Codes_1 <- renderUI({
    selectInput("Drug_Code_Select_1", "Drug Code #1", 
                choices = BNF_Dictionary[BNF_Dictionary$BNF_Level==input$BNF_Level,"Description"],
                            selected = "Antiepileptic drugs",
                selectize=TRUE)
  })
  
  output$Drug_Codes_2 <- renderUI({
    selectInput("Drug_Code_Select_2", "Drug Code #2", 
                choices = BNF_Dictionary[BNF_Dictionary$BNF_Level==input$BNF_Level,"Description"],
                            selected = "Drugs used in psychoses and related disorders")
  })
  
  #Call plots
  
  output$VolumePlot <- renderPlot({
     Year_Range <- input$Year_Range
     #Graph_IDs <- c("4.8", "4.2") #<-Drug_Codes
     Metric_to_Eval <- input$Metric_Evaluated
     
     Drug_Codes_Selected <- c(input$Drug_Code_Select_1, 
                              input$Drug_Code_Select_2)
     
     GraphScale <- YearColor %>% 
       filter(as.numeric(Year) >= Year_Range[1] & Year <=  Year_Range[2])

    
     Long_Raw_Data_Table %>%
       #filter(ID %in% Graph_IDs) %>%
       filter(Year >= Year_Range[1] & Year <=  Year_Range[2]) %>%
       filter(Description %in% Drug_Codes_Selected) %>%
       filter(Metric_Name == Metric_to_Eval) %>%
       ggplot(aes(x = Month, 
                  y = Value, 
                  color = factor(Year), 
                  #fill = factor(Year),
                  group = factor(Year))) +
       geom_line() +
       geom_point() +
       facet_grid(rows = vars(Description),
                  scales = "free_y",  # Let the width of facets vary and force all bars to have the same width.
                  switch = "x")  +    # Move the facet labels to the bottom.
       theme(axis.text.x = element_text(size = 10),
             axis.text.y = element_text(hjust = 0.5, size = 10),
             plot.title = element_text(hjust = 0.5, size = 30),
             legend.title = element_text(size = 25),
             legend.text = element_text(size = 15)) +
       labs(title= paste("Monthly", Metric_to_Eval, "By Drug Class By Year"),
            x ="Month",
            y = Metric_to_Eval,
            color = "Year") +
       #scale_y_continuous(labels=RelabelFunc) +
       scale_y_continuous(labels = function(x) format(x, scientific = TRUE)) +
       scale_x_discrete(limits = month.abb) +
       scale_color_manual(breaks = GraphScale$Year,
                         values= GraphScale$Color
       )
     
     

    
  }, height = 500, width = 800)
 
  output$YoYPlot <- renderPlot({
    Year_Range <- input$Year_Range
    Metric_to_Eval <- input$Metric_Evaluated
    Drug_Codes_Selected <- c(input$Drug_Code_Select_1, 
                             input$Drug_Code_Select_2)
    
    Long_YoY_Numbers %>% 
      filter(Year >= Year_Range[1] & Year <=  Year_Range[2]) %>%
      filter(Description %in% Drug_Codes_Selected) %>%
      filter(Metric_Name == Metric_to_Eval) %>%
      ggplot(aes(x = Month, 
                 y = Value, 
                 #color = Description, 
                 fill = Description,
                 group = Description
      )) +
      geom_bar(stat = "identity", position = "dodge") +
      facet_grid(rows = vars(factor(Year)),
                 scales = "free_x",  # Let the width of facets vary and force all bars to have the same width.
                 switch = "y")  +    # Move the facet labels to the bottom.
      theme(axis.text.x = element_text(size = 10),
            axis.text.y = element_text(hjust = 0.5, size = 10),
            plot.title = element_text(hjust = 0.5, size = 30),
            legend.title = element_text(size = 15),
            legend.text = element_text(size = 15),
            legend.position = "bottom") +
      labs(title=paste(Metric_to_Eval, "YoY Change By Drug Class"),
           x ="Month", 
           y = "% Change"
      ) +
      scale_x_discrete(limits = month.abb) +
      scale_y_continuous(labels = function(x) paste(x,"%",sep ="")) +
      scale_fill_manual(breaks = Drug_Codes_Selected,
                        values=c("purple", "orange")
      )
    
    
  }, height = 500, width = 800)
  
  output$YoYCompare <- renderPlot({
    Year_Range <- input$Year_Range
    Metric_to_Eval <- input$Metric_Evaluated
    
    GraphScale <- YearColor %>% 
      filter(as.numeric(Year) >= Year_Range[1] & Year <=  Year_Range[2])
    
    Drug_Codes_Selected <- c(input$Drug_Code_Select_1, 
                             input$Drug_Code_Select_2)
    
    
    YOY_Comparison_Table <- 
      YoY_Comparison_Long(Drug_Codes_Selected[1], 
                          Drug_Codes_Selected[2],
                          Metric_to_Eval)
    
    YOY_Comparison_Table %>% 
      filter(Year >= Year_Range[1] & Year <=  Year_Range[2]) %>%
      mutate (Year = factor(Year)) %>%
      ggplot(aes(x = Month, 
                 y = Value_Comparison,
                 #color = Year, 
                 fill = Year
                 #group = factor(Year)
      )) +
      geom_bar(stat = "identity", position = "dodge") +
      scale_x_discrete(limits = month.abb) +
      scale_y_continuous(labels = function(x) paste(x,"%",sep ="")) +
      theme(axis.text.x = element_text(size = 10),
            axis.text.y = element_text(hjust = 0.5, size = 10),
            plot.title = element_text(hjust = 0.5, size = 15),
            legend.title = element_text(size = 25),
            legend.text = element_text(size = 15)) +
      labs(title=paste("YoY Cost % Difference Between \n",
                       Drug_Codes_Selected[1], "&", 
                       Drug_Codes_Selected[2]),
           x ="Month", 
           y = "Percent Difference"
      ) +
      scale_fill_manual(breaks = GraphScale$Year,
                         values= GraphScale$Color
      )
    
    
    
    
    
   
   }, height = 500, width = 800)
   
  
  
}

##### UI #####


ui <- shinyUI(fluidPage(
  
  titlePanel("Open Prescribing Analysis"),
  
  sidebarLayout(
    sidebarPanel(
      
      p('User will select which level to look through descriptions'),
      p('(We can add a custom option for our own groupings too)'),
      
      selectInput("BNF_Level", "Selection Level",
                  c("Custom" = "Custom",
                    "Chapter" = "BNF chapter", 
                    "Section" = "BNF section", 
                    "Paragraph" = "BNF paragraph"),
                  selected = c("Section"= "BNF section")),
      
      p('Select Drug Code 1'),
      
      uiOutput("Drug_Codes_1"),

      p('Select Drug Code 2'),
      
      uiOutput("Drug_Codes_2"),
      
      selectInput("Metric_Evaluated", "Metric",
                  c("Items",
                    "Quantity",
                    "Cost"
                    )),
            
      sliderInput('Year_Range','Years Range',min=2016, max=2020, value=c(2018, 2020), step=1)
      
      
    ),
    
    mainPanel(
      p('User will select different tabs to see different graphs'),
      p('2016 has no YoY data'),
      hr(),
      tabsetPanel(
        tabPanel("Volume Plot", plotOutput("VolumePlot",width="100%")),
        tabPanel("YoY By Drug Class", plotOutput("YoYPlot",width="100%")),
        tabPanel("YoY Comparing Drug Classes ", plotOutput("YoYCompare",width="100%"))
      )
    )
  )
))  


##### Run #####
shinyApp(ui = ui, server = server)
