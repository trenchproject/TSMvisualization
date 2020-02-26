pkgs <- c('pdftools','tidyverse','taxize', 'raster','rgdal','plotly','reshape2','taxize', 'magrittr', 'TrenchR', 'ggplot2','ggridges','plotly', 'data.table', 'leaflet', 'shinyjs')
lapply(pkgs, library, character.only = TRUE)
#setwd("C:\\Users\\Bryan\\Google Drive\\TSMVisualization\\")
#setwd("C:\\Users\\lbuckley\\My Documents\\TSMviz")
setwd("/Volumes/GoogleDrive/Shared Drives/TrEnCh/TSMVisualization/")

month <- c(1:12)
names(month) <- c("January","February","March","April","May","June","July","August","September","October","November","December")
scenarios <- c("Normal","+1.5 °C","+2 °C")
hours <- c("12 AM","01 AM","02 AM","03 AM","04 AM","05 AM","06 AM","07 AM","08 AM","09 AM","10 AM","11 AM","12 PM","01 PM","02 PM","03 PM","04 PM","05 PM","06 PM","07 PM","08 PM","09 PM", "10 PM","11 PM")
org <- c("Takydromus sexlineatus","Coleonyx brevis","Holbrookia maculata","Lepidophyma flavimaculatum", "Psammodromus algirus", "Sceloporus undulatus", "Cophosaurus texanus", "Petrosaurus mearnsi","Platysaurus intermedius","Psammodromus hispanicus","Sceloporus magister","Tiliqua rugosa","Urosaurus ornatus")
variables <- c("Month", "Hour", "Scenario", "Shade")



shinyUI <- fluidPage (
  useShinyjs(),
  titlePanel("Thermal Safety Margin"),
  
  mainPanel(
    h3("Map (only the first seven species work)"),
    
    fluidRow(
      column(8, selectInput("select_species", label = "Species", choices = org, selected = "Coleonyx brevis")),
    ),
    
    hr(),
    
    fluidRow(
      column(4, radioButtons("rows", label = "Columns", choices = variables, inline = TRUE)),
      column(4, offset = 1, radioButtons("columns", label = "Rows", choices = variables, selected = "Hour", inline = TRUE))
      
    ),

    fluidRow(
      column(5, selectInput("select_month", label = "Month", choices = month, multiple = TRUE, selected = 1)),
      column(5, selectInput("select_hour", label = "Hour", choices = hours, multiple = TRUE, selected = "01 PM"))
      
    ),
    
    
    fluidRow(
      column(5, checkboxGroupInput("select_scenario", label = "Scenario", choices = scenarios, selected = "Normal")),
      column(5, checkboxGroupInput("select_shade", label = "Shade", choices = c("Covered", "Exposed"), selected = "Covered")),
      column(2, actionButton("run", "Run"))
      
    ),
    
    br(),
    
    fluidRow(
      column(8, offset = 1, leafletOutput("mymap")),
      column(2, offset = 1, actionButton("map_onoff", "Show/Hide map"))
    ),
    
    br(),
    
    fluidRow(
      column(12, plotOutput("plot1", width = "100%"))
    ), 
    
    br(), br(), br(), br(), br(), br(), br(), br(), br(), br(),
  
    
    hr(),
    
    h3("Plot"),
    
    fluidRow(
      column(8, selectInput("select_species_2", label = "Species", choices = org, selected = "Coleonyx brevis")),
    ),
    
    fluidRow(
      column(5, radioButtons("select_facet", label = "Facet", choices = c("Shade", "Scenario")))
    ),
    
    fluidRow(
      column(5, selectInput("select_month_2", label = "Month", choices = month)),
    ),
    
    fluidRow(
      column(12, plotOutput("plot2", width = "100%"))
    ), 
  )
)