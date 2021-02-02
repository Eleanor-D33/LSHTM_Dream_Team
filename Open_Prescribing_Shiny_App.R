########################################################
# Example in Rshiny for cost function                  #
# 29-Dec-2020                                          #
########################################################

library(shiny)
library(tidyverse)
library(ggplot2)


#BNF_Dictionary[BNF_Dictionary$type=="BNF section","id"]

# Define server logic for random distribution application
server <- function(input, output) {
  
  #
  output$Drug_Codes <- renderUI({
    selectInput("Drug_Codes", "Drug Code", 
                choices = as.character(BNF_Dictionary[BNF_Dictionary$id==input$BNF_Level,"id"]))
  })
  
  #Call plot
  
  output$VolumePlot <- renderPlot({
     Year_Range <- input$Year_Range
     Graph_IDs <- c("4.8", "4.2") #<-Drug_Codes
     Metric_to_Eval <- input$Metric_Evaluated
    
     Long_Raw_Data_Table %>%
       filter(ID %in% Graph_IDs) %>%
       filter(Year >= Year_Range[1] & Year <=  Year_Range[2]) %>%
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
       theme(axis.text.x = element_text(angle = 90, size = 10),
             axis.text.y = element_text(hjust = 0.5, angle = 90, size = 10),
             plot.title = element_text(hjust = 0.5, size = 30),
             legend.title = element_text(size = 25),
             legend.text = element_text(size = 15)) +
       labs(title= paste("Monthly", Metric_to_Eval, "By Drug Class By Year"),
            x ="Month",
            y = Metric_to_Eval,
            color = "Year") +
       scale_y_continuous(labels=RelabelFunc) +
       scale_x_discrete(limits = month.abb)
     
     

    
  }, height = 500, width = 800)
 
  output$YoYPlot <- renderPlot({
    Year_Range <- input$Year_Range
    Graph_IDs <- c("4.8", "4.2") #<-Drug_Codes
    Metric_to_Eval <- input$Metric_Evaluated
    
    Long_YoY_Numbers %>% 
      filter(ID %in% Graph_IDs) %>%
      filter(Year >= Year_Range[1] & Year <=  Year_Range[2]) %>%
      filter(Metric_Name == Metric_to_Eval) %>%
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
      theme(axis.text.x = element_text(angle = 90, size = 10),
            axis.text.y = element_text(hjust = 0.5, angle = 90, size = 10),
            plot.title = element_text(hjust = 0.5, size = 30),
            legend.title = element_text(size = 15),
            legend.text = element_text(size = 15),
            legend.position = "bottom") +
      labs(title=paste(Metric_to_Eval, "YoY Change By Drug Class"),
           x ="Month", 
           y = "% Change"
      ) +
      scale_x_discrete(limits = month.abb)
    
    
  }, height = 500, width = 800)
  
  output$YoYCompare <- renderPlot({
    Year_Range <- input$Year_Range
    Graph_IDs <- c("4.8", "4.2") #<-Drug_Codes
    Metric_to_Eval <- input$Metric_Evaluated
    
    YOY_Comparison_Long %>% 
      filter(Year >= Year_Range[1] & Year <=  Year_Range[2]) %>%
      filter(Metric_Name == Metric_to_Eval) %>%
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
                  c("Chapter" = "BNF chapter", 
                    "Section" = "BNF section", 
                    "Paragraph" = "BNF paragraph")),
      
      p('Descriptions will come up here for Drug Codes'),
      p('(Right now set to 4.2 (Antiepileptics) and 4.8 (Psychotics'),
      
      uiOutput("Drug_Codes"),
      
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
