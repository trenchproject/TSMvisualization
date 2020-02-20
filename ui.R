month <- c(1:12)
names(month) <- c("January","February","March","April","May","June","July","August","September","October","November","December")
scenarios <- c("Normal","+1.5 °C","+2 °C")
hours <- c("12 AM","01 AM","02 AM","03 AM","04 AM","05 AM","06 AM","07 AM","08 AM","09 AM","10 AM","11 AM","12 PM","01 PM","02 PM","03 PM","04 PM","05 PM","06 PM","07 PM","08 PM","09 PM", "10 PM","11 PM")
org <- c("Takydromus sexlineatus","Coleonyx brevis","Holbrookia maculata","Lepidophyma flavimaculatum","Petrosaurus mearnsi","Platysaurus intermedius","Psammodromus hispanicus","Sceloporus magister","Tiliqua rugosa","Urosaurus ornatus")
variables <- c("Month", "Hour", "Scenario", "Shade")

shinyUI <- fluidPage (
  titlePanel("Thermal Safety Margin"),
  
  mainPanel(
    h3("Map"),
    
    fluidRow(
      column(8, selectInput("select_species", label = "Species", choices = org, selected = "Takydromus sexlineatus")),
    ),
    
    fluidRow(
      column(5, selectInput("rows", label = "Columns", choices = variables)),
      column(5, selectInput("columns", label = "Rows", choices = variables, selected = "Hour"))
      
    ),
    
    fluidRow(
      column(5, selectInput("select_month", label = "Month", choices = month, multiple = TRUE, selected = 1)),
      column(5, selectInput("select_hour", label = "Hour", choices = hours, multiple = TRUE, selected = "01 PM"))
      
    ),
    
    fluidRow(
      column(5, selectInput("select_scenario", label = "Scenario", choices = scenarios, multiple = TRUE, selected = "Normal"))
      
    ),
    
    
    fluidRow(
      column(12, plotOutput("plot1", width = "100%"))
    ), 
    
    br(), br(), br(), br(), br(), br(), br(), br(), br(),
    hr(),
    
    h3("Plot"),
    
    fluidRow(
      column(8, selectInput("select_species_2", label = "Species", choices = org, selected = "Takydromus sexlineatus")),
    ),
    
    fluidRow(
      column(5, selectInput("select_month_2", label = "Month", choices = month)),
    ),
    
    fluidRow(
      column(12, plotOutput("plot2", width = "100%"))
    ), 
  )
)