month <- c(1:12)
names(month) <- c("January","February","March","April","May","June","July","August","September","October","November","December")
scenarios <- c("Normal","+1.5 °C","+2 °C")
hours <- c("12 AM","01 AM","02 AM","03 AM","04 AM","05 AM","06 AM","07 AM","08 AM","09 AM","10 AM","11 AM","12 PM","01 PM","02 PM","03 PM","04 PM","05 PM","06 PM","07 PM","08 PM","09 PM", "10 PM","11 PM")

#setwd("\\Volumes\\GoogleDrive\\Shared Drives\\TrEnCh\\TSMVisualization\\TSMdfs")
setwd("/Volumes/GoogleDrive/Shared Drives/TrEnCh/TSMVisualization/TSMdfs")

my_db <- src_sqlite("TSM data", create = FALSE)

shinyServer <- function(input, output) {
  
  # Method 1: Rda file
  # data_by_org <- reactive({
  #   org <- input$select_species
  #   spdata <- paste("TSMdfs\\",gsub(" ","_",org),"_combined.Rda", sep= "")
  #   load(spdata)
  #   combined
  # })
   
  
  # Method 2: fread from csv
  # data_by_org <- reactive({
  # 
  #   org <- input$select_species
  #   filename <- paste("TSMdfs\\",gsub(" ","_",org),"_combined.csv", sep= "")
  #   df <- data.table::fread(filename)
  # })
  
  
  # Method 3: RDS file
  # data_by_org <- reactive({
  #   
  #   org <- input$select_species
  #   filename <- paste("TSMdfs\\",gsub(" ","_",org),"_combined.rds", sep= "")
  #   df2 <- readRDS(filename)
  #   df2
  # })
  
  # Method 4: Database using dbWriteTable(), dbReadTable()
  # data_by_org <- reactive({
  #   org <- input$select_species
  #   path <- sub(" ","_",org)
  #   a <- dbReadTable(db, path)
  #   a[a$Shade=="Exposed\r",]$Shade <- "Exposed"
  #   a[a$Shade=="Covered\r",]$Shade <- "Covered"
  #   a
  # })
  
  # Method 5: database using src_sqlite
  dataInput <- eventReactive( input$run, {
    mm <- names(month[as.numeric(input$select_month)])
    org <- input$select_species
    tbl(my_db, paste(sub(" ","_",org), "_combined", sep = "")) %>% 
      filter(Hour %in% local(input$select_hour) & Month %in% mm & Scenario %in% local(input$select_scenario) & Shade %in% local(input$select_shade)) %>%
      tbl_df()
  }, ignoreNULL = FALSE
  )
  
  
  # data_by_org <- tbl(my_db, paste(sub(" ","_",org), "_combined", sep = "")) %>%
  #   filter(Hour %in% "01 PM" & Month %in% "January" & Scenario %in% "Normal" & Shade %in% "Covered") %>% 
  #   tbl_df()
  
  # data_by_org_2 <- reactive({
  #   org <- input$select_species_2
  #   spdata <- paste("TSMdfs\\",gsub(" ","_",org),"_combined.Rda", sep= "")
  #   load(spdata)
  #   combined
  # })
  
  # data_by_org_2 <- reactive({
  #   org_2 <- input$select_species_2
  #   filename <- paste("TSMdfs\\",gsub(" ","_",org_2),"_combined.csv", sep= "")
  #   df <- data.table::fread(filename)
  #   levels(df$Hour) <- hours
  #   levels(df$Month) <- month
  #   df
  # })
  
  # data_by_org_2 <- reactive({
  #   org_2 <- input$select_species_2
  #   filename <- paste("TSMdfs\\",gsub(" ","_",org),"_combined.rds", sep= "")
  #   df2 <- readRDS(filename)
  #   df2
  # })
  
  # data_by_org_2 <- reactive({
  #   org <- input$select_species_2
  #   path <- sub(" ","_",org)
  #   a <- dbReadTable(db, path)
  #   a[a$Shade=="Exposed\r",]$Shade <- "Exposed"
  #   a[a$Shade=="Covered\r",]$Shade <- "Covered"
  #   a
  # })
  
  
  dataInput_2 <- reactive({
    mm <- names(month[as.numeric(input$select_month_2)])
    org <- input$select_species_2
    df <- tbl(my_db, paste(sub(" ","_",org), "_combined", sep = "")) %>% 
            filter(Month %in% mm) %>%
            tbl_df()
    levels(df$Hour) <- hours
    levels(df$Month) <- month
    df
  })
  

  # dataInput <- reactive({
  #   data_by_org() %>% filter(Hour %in% input$select_hour & Month %in% names(month[as.numeric(input$select_month)]) & Scenario %in% input$select_scenario & Shade %in% input$select_shade)
  # })
  
  # dataInput_2 <- reactive({
  #   data_by_org_2() %>% filter(Month %in% names(month[as.numeric(input$select_month_2)]))
  # })
  
  x_variable <- eventReactive( input$run, {
    switch(input$columns, Month = "factor(Month, levels = names(month))", Hour = "Hour", Scenario = "Scenario", Shade = "Shade")
  }, ignoreNULL = FALSE
  )
  
  y_variable <- eventReactive( input$run, {
    switch(input$rows, Month = "Month", Hour = "Hour", Scenario = "Scenario", Shade = "Shade")
  }, ignoreNULL = FALSE
  )
  
  title <- eventReactive( input$run, {
    title_m <- paste("|", names(month[as.numeric(input$select_month)]))
    title_h <- paste("|", input$select_hour)
    title_sc <- paste("|", input$select_scenario)
    title_sh <- paste("|", input$select_shade)
    
    if (as.character(x_variable()) == "Month" | as.character(y_variable()) == "Month") {
      title_m <- ""
    } 
    if (as.character(x_variable()) == "Hour" | as.character(y_variable()) == "Hour") {
      title_h <- ""
    } 
    if (as.character(x_variable()) == "Scenario" | as.character(y_variable()) == "Scenario") {
      title_sc <- ""
    } 
    if (as.character(x_variable()) == "Shade" | as.character(y_variable()) == "Shade") {
      title_sh <- ""
    }     
    paste(input$select_species, title_m, title_h, title_sc, title_sh)
  }, ignoreNULL = FALSE
  )
  
  output$mymap <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = (max(dataInput()$x) + min(dataInput()$x))/2,  lat = (max(dataInput()$y) + min(dataInput()$y))/2, zoom = 3) %>%
      addRectangles(lng1 = min(dataInput()$x), lng2 = max(dataInput()$x), lat1 = min(dataInput()$y), lat2 = max(dataInput()$y))
      
  })
  
  observeEvent(input$map_onoff,{
    toggle("mymap")
  })
  
  
  output$plot1 <- renderPlot({
    
    facet_formula <- as.formula(paste(x_variable(), "~", y_variable()))
    
    ggplot() +
      borders(fill="grey",colour="black") +
      ggtitle(title()) + xlab("Longitude") + ylab("Latitude") +
      geom_raster(data=dataInput(), aes(x = x, y = y, fill = Tsm)) + scale_fill_gradient(low = "red", high = "blue", name = "Thermal Safety \nMargin (°C)") +
      facet_grid(facet_formula) +
      coord_quickmap(xlim = c(min(dataInput()$x), max(dataInput()$x)), ylim = c(min(dataInput()$y), max(dataInput()$y)),expand = TRUE) +
      theme_bw( ) + theme(strip.text = element_text(size = 12)) +
      theme(plot.title = element_text(size = 16), axis.text = element_text(size = 12), axis.title = element_text(size = 14), legend.text = element_text(size = 12), legend.title = element_text(size = 12))
  }, height = 600, width = 900)
    
  
  
  output$plot2 <- renderPlot({
    
    if (input$select_facet == "Shade") {
      ggplot(dataInput_2(), aes(x=Tsm, y= Hour)) + xlab("Thermal Safety Margin (°C)") + 
        geom_density_ridges2(aes(fill=Scenario), rel_min_height = 0.01, scale=2, alpha=0.5) + ggtitle(paste(input$select_species_2,"|", names(month[as.numeric(input$select_month_2)]))) + 
        scale_y_discrete(limits = rev(levels(dataInput_2()$Hour)))  + theme_bw() + facet_wrap(~Shade) + theme(strip.text = element_text(size = 12)) +
        theme(plot.title = element_text(size = 16), axis.text = element_text(size = 12), axis.title = element_text(size = 14), legend.text = element_text(size = 12), legend.title = element_text(size = 12))
      
    } else {
      ggplot(dataInput_2(),aes(x=Tsm, y= Hour)) + xlab("Thermal Safety Margin (°C)") + 
        geom_density_ridges2(aes(fill=Shade),rel_min_height = 0.01 ,scale=2,alpha=0.5) + ggtitle(paste(input$select_species_2,"|", names(month[as.numeric(input$select_month_2)]))) + 
        scale_y_discrete(limits = rev(levels(dataInput_2()$Hour)))  + theme_bw() + facet_wrap(~factor(Scenario, levels = scenarios)) + scale_fill_manual(values = c("blue","red")) +
        theme(strip.text = element_text(size = 12)) +
        theme(plot.title = element_text(size = 16), axis.text = element_text(size = 12), axis.title = element_text(size = 14), legend.text = element_text(size = 12), legend.title = element_text(size = 12))
    }
    
    
  }, height = 600, width = 800)  
  
}