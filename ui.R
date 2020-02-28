pkgs <- c('pdftools','tidyverse','taxize', 'raster','rgdal','plotly','reshape2','taxize', 'magrittr', 'TrenchR', 'ggplot2','ggridges','plotly', 'data.table', 'leaflet', 'shinyjs')
lapply(pkgs, library, character.only = TRUE)

#setwd("/Volumes/GoogleDrive/Shared Drives/TrEnCh/TSMVisualization/")

month <- c(1:12)
names(month) <- c("January","February","March","April","May","June","July","August","September","October","November","December")
scenarios <- c("Normal","+1.5 °C","+2 °C")
hours <- c("12 AM","01 AM","02 AM","03 AM","04 AM","05 AM","06 AM","07 AM","08 AM","09 AM","10 AM","11 AM","12 PM","01 PM","02 PM","03 PM","04 PM","05 PM","06 PM","07 PM","08 PM","09 PM", "10 PM","11 PM")
org <- c("Takydromus sexlineatus","Coleonyx brevis","Psammodromus hispanicus", "Holbrookia maculata","Lepidophyma flavimaculatum", "Psammodromus algirus", "Sceloporus undulatus", "Cophosaurus texanus", "Petrosaurus mearnsi","Platysaurus intermedius","Psammodromus hispanicus","Sceloporus magister","Tiliqua rugosa","Urosaurus ornatus")
variables <- c("Month", "Hour", "Scenario", "Shade")

#lizards_tpc <- fread("lizards_tpc.csv")



shinyUI <- fluidPage (
  useShinyjs(),
  titlePanel("Climate Change and Lizards"),
  
  tabsetPanel(type = "tabs", 
    tabPanel("Distribution Map",
      sidebarLayout(
        sidebarPanel(
          h3("Distribution map"),
          p("Select a species and hit \"Run\" to take a look at their thermal safety margins depending on various conditions within their habitat."),
          selectInput("species_1", label = "Species", choices = org, selected = "Coleonyx brevis"),
          
          fluidRow(
            column(6,radioButtons("rows", label = "Horizontal facets", choices = variables)),
            column(6,radioButtons("columns", label = "Vertical facets", choices = variables, selected = "Hour"))
          ),
          
          selectInput("month", label = "Month", choices = month, multiple = TRUE, selected = 1),
          selectInput("hour", label = "Hour", choices = hours, multiple = TRUE, selected = "01 PM"),
          
          fluidRow(
            column(6, checkboxGroupInput("scenario", label = "Scenario", choices = scenarios, selected = "Normal")),
            column(6, checkboxGroupInput("shade", label = "Shade", choices = c("Covered", "Exposed"), selected = "Covered"))
          ),  
          
          fluidRow(
            column(2, actionButton("map_onoff", "Show/Hide map")),
            column(2, offset = 4, actionButton("run", "Run"))
            
          ),
        ),
        mainPanel(
        
          fluidRow(
            column(12, plotOutput("plot1", width = "100%"))
          ), 
          
          br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(),
          
        
          fluidRow(
            column(8, leafletOutput("mymap"))
          ),
          
          br(),
        )
      )
    ),
    
    tabPanel("Thermal Performance Curve",
      sidebarLayout(
        sidebarPanel(
    
          h3("Thermal performance curve"),
          p("Select a species to look at their thermal performance curve and how varying environmental conditions affects their performance."),
          selectInput("species_tpc", label = "Species", choices = lizards_tpc$Binomial), 
          selectInput("month_tpc", label = "Month", choices = month, selected = 1),
          selectInput("hour_tpc", label = "Hour", choices = hours, selected = "01 PM"),
          
          
          fluidRow(
            column(6, radioButtons("scenario_tpc", label = "Scenario", choices = scenarios, selected = "Normal")),
            column(6, radioButtons("shade_tpc", label = "Shade", choices = c("Covered", "Exposed"), selected = "Covered")),
          )
        ),
        
        mainPanel(
          br(),
          plotOutput("TPC")
        )
      ),
      br(),
    ),
    
    tabPanel("Plot", 
      sidebarLayout(
        sidebarPanel(
          h3("Plot"),
          
          selectInput("species_2", label = "Species", choices = org, selected = "Coleonyx brevis"),
          
          radioButtons("facet", label = "Facet", choices = c("Shade", "Scenario"), inline = TRUE),
        
          selectInput("month_2", label = "Month", choices = month)
        ),
        
        mainPanel(
          plotOutput("plot2", width = "100%")
          
        )
      )
    ),
    
    tabPanel("Data", 
      sidebarLayout(
        sidebarPanel(
          h3("Data"),
          
          selectInput("species_data", label = "Species", choices = org, selected = "Coleonyx brevis"),
          
          selectInput("month_data", label = "Month", choices = month),
          selectInput("hour_data", label = "Hour", choices = hours, selected = "01 PM"),
          
          
          fluidRow(
            column(6, radioButtons("scenario_data", label = "Scenario", choices = scenarios, selected = "Normal")),
            column(6, radioButtons("shade_data", label = "Shade", choices = c("Covered", "Exposed"), selected = "Covered")),
          )
        ),
          
        
        
        mainPanel(
          textOutput("text_data")
        )
      )
    )
  )
)