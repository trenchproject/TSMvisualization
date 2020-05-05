pkgs <- c('shiny', 'tidyverse','taxize', 'raster','plotly','reshape2', 'magrittr', 'ggplot2','ggridges','TrenchR','plotly', 'data.table', 'leaflet', 'shinyjs', 'scales', 'shinysky', 'shinythemes', 'shinyWidgets', 'maps', 'shinycssloaders')
# 'pdftools', 'rgdal'
lapply(pkgs, library, character.only = TRUE)

month <- c(1:12)
names(month) <- c("January","February","March","April","May","June","July","August","September","October","November","December")
scenarios <- c("Normal","+1.5 °C","+2 °C")
hours <- c("12 AM","01 AM","02 AM","03 AM","04 AM","05 AM","06 AM","07 AM","08 AM","09 AM","10 AM","11 AM","12 PM","01 PM","02 PM","03 PM","04 PM","05 PM","06 PM","07 PM","08 PM","09 PM", "10 PM","11 PM")

org_done <- c("Sceloporus occidentalis", "Uta stansburiana")
org_tpc <- c("Anolis carolinensis",
             "Ctenotus regius", "Ctenotus taeniolatus", "Ctenotus uber", "Dipsosaurus dorsalis", "Elgaria multicarinata", "Eulamprus kosciuskoi", 
             "Eulamprus tympanum", "Hemiergis decresiensis", "Lepidophyma flavimaculatum", "Platysaurus intermedius", "Podarcis muralis", "Psammodromus algirus", 
             "Psammodromus hispanicus", "Pseudemoia entrecasteauxii", "Sceloporus graciosus", "Sceloporus variabilis", 
             "Sphaerodactylus macrolepis", "Sphaerodactylus nicholsi", "Takydromus septentrionalis", "Takydromus sexlineatus", "Xantusia vigilis")
org <- c(org_tpc, "Coleonyx brevis")

variables <- c("Month", "Hour", "Scenario", "Shade")

lizards_tpc <- fread("lizards_tpc.csv")


shinyUI <- fluidPage (theme = shinytheme("united"),
                      
                      #tags$head(tags$style(HTML('* {font-family: Calibri;}'))),
                      setBackgroundColor(color = "#F5F5F5"),
                      
                      useShinyjs(),
                      
                      titlePanel(
                        div(tags$img(src="lizard_pic.png", height = 50), 
                            "Climate Change and Lizards ")
                      ),
                      
                      hr(),
                      
                      includeHTML("intro.html"),
                      
                      br(), 
                      
                      strong("Select a species and explore their distribution, current status and the risk they may face from increasing temperature."),
                      
                      fluidRow(
                        column(6, selectInput("species", label = "", choices = list("Thermal info added" = org_done, "Not added" = org), selected = "Sceloporus occidentalis"))
                      ),
                      
                      htmlOutput("species_info") %>% withSpinner(type = 7),
                      
                      tabsetPanel(type = "tabs",
                                  
                                  tabPanel("Distribution Map",
                                           sidebarLayout(
                                             sidebarPanel(
                                               h3("Distribution map"),
                                               p("Set the variables and hit \"Run\" to take a look at the thermal safety margins of the selected species within their distribution."),
                                               p("Click on the map to get a more accurate data on TSM of that location."),
                                               
                                               # fluidRow(
                                               #   column(10, radioButtons("dist_frac", label = "Show", choices = c("Distribution", "Fraction"), selected = "Distribution"))
                                               # ),
                                               hr(),
                                               fluidRow(
                                                 column(6,radioButtons("rows", label = "Horizontal facets", choices = variables)),
                                                 column(6,radioButtons("columns", label = "Vertical facets", choices = variables, selected = "Hour"))
                                               ),
                                               
                                               select2Input("month", label = "Month", choices = names(month), multiple = TRUE, selected = "January"),
                                               select2Input("hour", label = "Hour", choices = hours, multiple = TRUE, selected = "01 PM"),
                                               
                                               fluidRow(
                                                 column(6, checkboxGroupInput("scenario", label = "Scenario", choices = scenarios, selected = "Normal")),
                                                 column(6, checkboxGroupInput("shade", label = "Shade", choices = c("Exposed", "Covered"), selected = "Exposed"))
                                               ),  
                                               
                                               fluidRow(
                                                 column(8, actionButton("map_onoff", "Show/Hide world map", styleclass = "success")),
                                                 column(2, actionButton("run", "Run", styleclass = "primary"))
                                               ),
                                             ),
                                             
                                             mainPanel(
                                               fluidRow(column(12, plotOutput("plot1", click = "plot_click") %>% withSpinner(type = 7))),
                                               br(), 
                                               strong("Operative temperature and TSM of the clicked location"),
                                               verbatimTextOutput("info"),
                                               strong("Distribution of TSM"),
                                               fluidRow(column(12, plotOutput("density") %>% withSpinner(type = 7))),
                                               column(8, offset = 2, align="center", leafletOutput("mymap")),
                                               
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
                                                 column(6, radioButtons("shade_tpc", label = "Shade", choices = c("Exposed", "Covered"), selected = "Exposed")),
                                               )
                                             ),
                                             
                                             mainPanel(
                                               conditionalPanel(condition = 'input.species' %in% lizards_tpc$Binomial, 
                                               #br(), br(), br(), br(),
                                               plotOutput("TPC") %>% withSpinner(type = 7)),
                                               conditionalPanel(condition = !('input.species' %in% lizards_tpc$Binomial), p("No Data"))
                                             )
                                           ),
                                           br(),
                                  ),
                                  
                                  tabPanel("Plot", 
                                           sidebarLayout(
                                             sidebarPanel(
                                               h3("Plot"),
                                               p("Explore the change in thermal safety margins of the selected species throughout the day in different months."),
                                               p("The shapes on the plot represent the frequency of TSM within the species."),
                                               
                                               radioButtons("facet", label = "Facet", choices = c("Shade", "Scenario"), inline = TRUE),
                                               
                                               selectInput("month_2", label = "Month", choices = month)
                                             ),
                                             
                                             mainPanel(
                                               column(12, align = "center", plotOutput("plot2", width = "100%") %>% withSpinner(type = 7))
                                             )
                                           )
                                  )
                                  
                                  # tabPanel("Raw Data", 
                                  #          sidebarLayout(
                                  #            sidebarPanel(
                                  #              h3("Raw Data"),
                                  #              p("Get the numric data on thermal safety margins here."),
                                  #              p("Select \"Annual\" and set TSM to 100 to see all our data."),
                                  #              
                                  #              hr(),
                                  #              
                                  #              h5("Show results for"),
                                  #              
                                  #              selectInput("month_data", label = "Month", choices = c(month, "Annual")),
                                  #              
                                  #              numericInput("value", label = "TSM Less than", value = 0),
                                  #              
                                  #              fluidRow(
                                  #                column(6, radioButtons("scenario_data", label = "Scenario", choices = scenarios, selected = "Normal")),
                                  #                column(6, radioButtons("shade_data", label = "Shade", choices = c("Exposed", "Covered"), selected = "Exposed")),
                                  #              )
                                  #            ),
                                  #            
                                  #            mainPanel(
                                  #              br(),
                                  #              h4("Mean thermal safety margins across the distibution"),
                                  #              htmlOutput("text_data") %>% withSpinner(type = 7)
                                  #            )
                                  #          )
                                  # )
                      )
)