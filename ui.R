pkgs <- c('pdftools','tidyverse','taxize', 'raster','rgdal','plotly','reshape2','taxize', 'magrittr', 'TrenchR', 'ggplot2','ggridges','plotly', 'data.table', 'leaflet', 'shinyjs', 'scales')
lapply(pkgs, library, character.only = TRUE)

#setwd("/Volumes/GoogleDrive/Shared Drives/TrEnCh/TSMVisualization/")
setwd("C:\\Users\\lbuckley\\My Documents\\TSMviz")

month <- c(1:12)
names(month) <- c("January","February","March","April","May","June","July","August","September","October","November","December")
scenarios <- c("Normal","+1.5 °C","+2 °C")
hours <- c("12 AM","01 AM","02 AM","03 AM","04 AM","05 AM","06 AM","07 AM","08 AM","09 AM","10 AM","11 AM","12 PM","01 PM","02 PM","03 PM","04 PM","05 PM","06 PM","07 PM","08 PM","09 PM", "10 PM","11 PM")

org_tpc <- c("Psammodromus hispanicus", "Psammodromus algirus", "Anolis carolinensis", "Ctenotus regius", "Ctenotus taeniolatus", "Ctenotus uber",  
             "Dipsosaurus dorsalis", "Elgaria multicarinata", "Eulamprus kosciuskoi", "Eulamprus tympanum", "Hemiergis decresiensis", "Lepidophyma flavimaculatum",
             "Platysaurus intermedius", "Podarcis muralis", "Pseudemoia entrecasteauxii", "Sceloporus graciosus", "Sceloporus occidentalis", 
             "Sceloporus variabilis", "Sphaerodactylus macrolepis", "Sphaerodactylus nicholsi", "Takydromus septentrionalis", "Takydromus sexlineatus", "Uta stansburiana", "Xantusia vigilis")
org <- c(org_tpc, "Coleonyx brevis")

variables <- c("Month", "Hour", "Scenario", "Shade")

lizards_tpc <- fread("lizards_tpc.csv")


shinyUI <- fluidPage (
  useShinyjs(),
  titlePanel("Climate Change and Lizards"),
  
  hr(),
  
  h4("What is Thermal Safety Margin (TSM)?"),
  
  textOutput("introduction"),
  br(),
  plotOutput("intro_fig", width = "100%"),
  
  br(), 
  
  strong("Select a species and explore their distribution, current status and the risk they face from increasing temperature"),
  
  fluidRow(
    column(6, selectInput("species", label = "", choices = org_tpc, selected = "Psammodromus hispanicus"))
  ),
  
  htmlOutput("species_info"),
  
  tabsetPanel(type = "tabs", 
              tabPanel("Distribution Map",
                       sidebarLayout(
                         sidebarPanel(
                           h3("Distribution map"),
                           p("Set the variables and hit \"Run\" to take a look at the thermal safety margins of the selected species within their distribution."),
                           p("Selecting \"Fraction\" will display the percentage of occurence of each TSMs over the entire distribution."),
                           
                           fluidRow(
                             column(10, radioButtons("dist_frac", label = "Show", choices = c("Distribution", "Fraction"), selected = "Distribution"))
                           ),
                           hr(),
                           fluidRow(
                             column(6,radioButtons("rows", label = "Horizontal facets", choices = variables)),
                             column(6,radioButtons("columns", label = "Vertical facets", choices = variables, selected = "Hour"))
                           ),
                           
                           selectInput("month", label = "Month", choices = month, multiple = TRUE, selected = 1),
                           selectInput("hour", label = "Hour", choices = hours, multiple = TRUE, selected = "01 PM"),
                           
                           fluidRow(
                             column(6, checkboxGroupInput("scenario", label = "Scenario", choices = scenarios, selected = "Normal")),
                             column(6, checkboxGroupInput("shade", label = "Shade", choices = c("Exposed", "Covered"), selected = "Exposed"))
                           ),  
                           
                           fluidRow(
                             column(8, actionButton("map_onoff", "Show/Hide world map")),
                             column(2, actionButton("run", "Run"))
                           ),
                         ),
                         
                         mainPanel(
                           conditionalPanel(
                             condition = "input.dist_frac == 'Distribution'", plotOutput("plot1", width = "100%")
                           ),
                           conditionalPanel(
                             condition = "input.dist_frac == 'Fraction'", plotOutput("density")
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
                           p("Take a look at the selected species' thermal performance curve and how varying environmental conditions affect their performance."),
                           
                           selectInput("month_tpc", label = "Month", choices = month, selected = 1),
                           selectInput("hour_tpc", label = "Hour", choices = hours, selected = "01 PM"),
                           
                           fluidRow(
                             column(6, radioButtons("scenario_tpc", label = "Scenario", choices = scenarios, selected = "Normal")),
                             column(6, radioButtons("shade_tpc", label = "Shade", choices = c("Covered", "Exposed"), selected = "Covered")),
                           )
                         ),
                         
                         mainPanel(
                           br(), br(), br(), br(),
                           plotOutput("TPC")
                         )
                       ),
                       br(),
              ),
              
              tabPanel("Plot", 
                       sidebarLayout(
                         sidebarPanel(
                           h3("Plot"),
                           p("Explore the change in thermal safety margins of the selected species throughout the day in different months."),
                           p("The shapes on the plot represent the variation of TSM within the species due to the varying habitats of individuals."),
                           
                           radioButtons("facet", label = "Facet", choices = c("Shade", "Scenario"), inline = TRUE),
                           
                           selectInput("month_2", label = "Month", choices = month)
                         ),
                         
                         mainPanel(
                           plotOutput("plot2", width = "100%")
                         )
                       )
              ),
              
              tabPanel("Raw Data", 
                       sidebarLayout(
                         sidebarPanel(
                           h3("Raw Data"),
                           p("Get the numric data on thermal safety margins here."),
                           p("Select \"Annual\" and set TSM to 100 to see all our data."),
                           
                           hr(),
                           
                           h5("Show results for"),
                           
                           selectInput("month_data", label = "Month", choices = c(month, "Annual")),
                           
                           numericInput("value", label = "TSM Less than", value = 0),
                           
                           fluidRow(
                             column(6, radioButtons("scenario_data", label = "Scenario", choices = scenarios, selected = "Normal")),
                             column(6, radioButtons("shade_data", label = "Shade", choices = c("Covered", "Exposed"), selected = "Covered")),
                           )
                         ),
                         
                         mainPanel(
                           br(),
                           h4("Mean thermal safety margins across the distibution"),
                           htmlOutput("text_data")
                         )
                       )
              )
  )
)