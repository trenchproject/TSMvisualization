month <- c(1:12)
names(month) <- c("January","February","March","April","May","June","July","August","September","October","November","December")
scenarios <- c("Normal","+1.5 °C","+2 °C")
hours <- c("12 AM","01 AM","02 AM","03 AM","04 AM","05 AM","06 AM","07 AM","08 AM","09 AM","10 AM","11 AM","12 PM","01 PM","02 PM","03 PM","04 PM","05 PM","06 PM","07 PM","08 PM","09 PM", "10 PM","11 PM")

#setwd("/Volumes/GoogleDrive/Shared Drives/TrEnCh/TSMVisualization/TSMdfs")

lizards_tpc <- fread("lizards_tpc.csv")


shinyServer <- function(input, output) {
  
  #Method 1: Rda file
  # data_by_org <- reactive({
  #   org <- input$species
  #   spdata <- paste("TSMdfs\\",gsub(" ","_",org),"_combined.Rda", sep= "")
  #   load(spdata)
  #   df <- combined
  #   df$Month <- factor(df$Month, levels = names(month))
  #   df$Hour <- factor(df$Hour, levels = hours)
  #   df$Scenario <- factor(df$Scenario, levels = scenarios)
  #   df
  # })
  
  #Method 2: fread from csv
  # data_by_org <- eventReactive( input$run,{
  # 
  #   org <- input$species_1
  #   filename <- paste("Data\\",gsub(" ","_",org),"_combined.csv", sep= "")
  #   df <- data.table::fread(filename)
  #   df$Month <- factor(df$Month, levels = names(month))
  #   df$Hour <- factor(df$Hour, levels = hours)
  #   df$Scenario <- factor(df$Scenario, levels = scenarios)
  #   df
  # }, ignoreNULL = FALSE
  # )
  
  #Method 3: RDS file
  data_by_org <- eventReactive( input$run,{

    org <- input$species_1
    filename <- paste("Data\\",gsub(" ","_",org),"_combined.rds", sep= "")
    df <- readRDS(filename)
    df$Month <- factor(df$Month, levels = names(month))
    df$Hour <- factor(df$Hour, levels = hours)
    df$Scenario <- factor(df$Scenario, levels = scenarios)
    df
  }, ignoreNULL = FALSE
  )
  
  # Method 4: Database using dbWriteTable(), dbReadTable()
  # data_by_org <- reactive({
  #   org <- input$species
  #   path <- sub(" ","_",org)
  #   a <- dbReadTable(db, path)
  #   a[a$Shade=="Exposed\r",]$Shade <- "Exposed"
  #   a[a$Shade=="Covered\r",]$Shade <- "Covered"
  #   a
  # })
  
  # # Method 5: database using src_sqlite
  # dataInput <- eventReactive( input$run, {
  #   mm <- names(month[as.numeric(input$month)])
  #   org <- input$species
  #   tbl(my_db, paste(sub(" ","_",org), "_combined", sep = "")) %>% 
  #     filter(Hour %in% local(input$hour) & Month %in% mm & Scenario %in% local(input$scenario) & Shade %in% local(input$shade)) %>%
  #     tbl_df()
  # }, ignoreNULL = FALSE
  # )
  
  
  # data_by_org_2 <- reactive({
  #   org <- input$species_2
  #   spdata <- paste("TSMdfs\\",gsub(" ","_",org),"_combined.Rda", sep= "")
  #   load(spdata)
  #   combined
  # })
  
  # data_by_org_2 <- reactive({
  #   org_2 <- input$species_2
  #   filename <- paste("Data\\",gsub(" ","_",org_2),"_combined.csv", sep= "")
  #   df <- data.table::fread(filename)
  #   levels(df$Hour) <- hours
  #   levels(df$Month) <- month
  #   df
  # })
  
  data_by_org_2 <- reactive({
    # org_2 <- "Xantusia vigilis"
    # org_2 <- "Psammodromus hispanicus"
    org_2 <- input$species_2
    filename <- paste("Data\\",gsub(" ","_",org_2),"_combined.rds", sep= "")
    df2 <- readRDS(filename)
    # levels(df2$Hour) <- hours
    # levels(df2$Month) <- month
    df2$Month <- factor(df2$Month, levels = names(month))
    df2$Hour <- factor(df2$Hour, levels = hours)
    df2$Scenario <- factor(df2$Scenario, levels = scenarios)
    df2
  })
  
  # data_by_org_2 <- reactive({
  #   org <- input$species_2
  #   path <- sub(" ","_",org)
  #   a <- dbReadTable(db, path)
  #   a[a$Shade=="Exposed\r",]$Shade <- "Exposed"
  #   a[a$Shade=="Covered\r",]$Shade <- "Covered"
  #   a
  # })
  
  
  # dataInput_2 <- reactive({
  #   mm <- names(month[as.numeric(input$month_2)])
  #   org <- input$species_2
  #   df <- tbl(my_db, paste(sub(" ","_",org), "_combined", sep = "")) %>% 
  #           filter(Month %in% mm) %>%
  #           tbl_df()
  #   levels(df$Hour) <- hours
  #   levels(df$Month) <- month
  #   df
  # })
  

  dataInput <- eventReactive( input$run,{
    data_by_org() %>% filter(Hour %in% input$hour & Month %in% names(month[as.numeric(input$month)]) & Scenario %in% input$scenario & Shade %in% input$shade)
  }, ignoreNULL = FALSE
  )
  
  
  dataInput_2 <- reactive({
    data_by_org_2() %>% filter(Month %in% names(month[as.numeric(input$month_2)]))
  })
  
  #df2 <- df2 %>% filter(Month %in% 1)

  
  dataTPC <- reactive({
    org <- input$species_tpc
    filename <- paste("Data\\",gsub(" ","_",org),"_combined.rds", sep= "")
    df <- readRDS(filename) %>% filter(Hour %in% input$hour_tpc & Month %in% names(month[as.numeric(input$month_tpc)]) & Scenario %in% input$scenario_tpc & Shade %in% input$shade_tpc)
    lizards_tpc[lizards_tpc$Binomial == input$species_tpc, Tmax] - mean(df$Tsm)
  })
  
  x_variable <- eventReactive( input$run, {
    switch(input$columns, Month = "Month", Hour = "Hour", Scenario = "Scenario", Shade = "Shade")
  }, ignoreNULL = FALSE
  )
  
  y_variable <- eventReactive( input$run, {
    switch(input$rows, Month = "Month", Hour = "Hour", Scenario = "Scenario", Shade = "Shade")
  }, ignoreNULL = FALSE
  )
  
  title <- eventReactive( input$run, {
    title_m <- paste("| ", names(month[as.numeric(input$month)]))
    title_h <- paste("| ", input$hour)
    title_sc <- paste("| ", input$scenario)
    title_sh <- paste("| ", input$shade)
    
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
    paste(input$species_1, title_m, title_h, title_sc, title_sh)
  }, ignoreNULL = FALSE
  )
  
  output$mymap <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = (max(dataInput()$x) + min(dataInput()$x))/2,  lat = (max(dataInput()$y) + min(dataInput()$y))/2, zoom = 3) %>%
      addRectangles(lng1 = min(dataInput()$x), lng2 = max(dataInput()$x), lat1 = min(dataInput()$y), lat2 = max(dataInput()$y))
      
  })
  
  observeEvent(input$map_onoff,{
    toggle("mymap")
  })
  
  
  output$plot1 <- renderPlot({
    
    facet_formula <- as.formula(paste(x_variable(), "~", y_variable()))
    #facet_formula <- as.formula(paste("factor(\"Month\", levels = names(month)) ~", y_variable()))
    
    ggplot() +
      borders(fill="grey",colour="black") +
      ggtitle(title()) + xlab("Longitude (°)") + ylab("Latitude (°)") +
      geom_raster(data=dataInput(), aes(x = x, y = y, fill = Tsm)) + scale_fill_gradient(low = "red", high = "blue", name = "Thermal Safety \nMargin (°C)") +
      facet_grid(facet_formula) + coord_quickmap(xlim = c(min(dataInput()$x), max(dataInput()$x)), ylim = c(min(dataInput()$y), max(dataInput()$y)),expand = TRUE) +
      theme_bw( ) + theme(strip.text = element_text(size = 12)) +
      theme(plot.title = element_text(size = 18), axis.text = element_text(size = 12), axis.title = element_text(size = 14), legend.text = element_text(size = 12),
            legend.title = element_text(size = 12))
    
  }, height = 650, width = 650)
    
  

  output$TPC <- renderPlot({
    lizards_tpc <- lizards_tpc[lizards_tpc$Binomial == input$species_tpc, ]
    curve <- TPC(0:pmax(50, dataTPC()), lizards_tpc$Topt, as.numeric(as.character(lizards_tpc$tmin)), lizards_tpc$Tmax)
    
    ggplot() + geom_line(data = NULL, aes(x = c(0:pmax(50, dataTPC())), y = curve), col = "steelblue", size = 1.2) + xlab("Temperature (°C)") + ylab("Performance") +
      geom_segment(aes(x = lizards_tpc$Tmax - 5, y = 0.1, xend = lizards_tpc$Tmax, yend = 0), size = 1) + geom_text(aes(x = lizards_tpc$Tmax - 6, y = 0.14, label = "Tmax"), size = 5) +
      geom_vline(xintercept = dataTPC(), size = 1.2) + geom_text(aes(x = dataTPC() - 1.7, label = "Operative temperature", y = 0.45), size = 5, angle = 90) + theme_classic( ) +
      geom_rect(aes(xmin = lizards_tpc$Tmax, xmax = Inf, ymin = -Inf, ymax = Inf), alpha = 0.7, fill = "red") +
      scale_x_continuous(limits = c(0,pmax(50, dataTPC())+2), expand = c(0,0)) + scale_y_continuous(limits = c(0, 1.05), expand = c(0,0)) +
      theme(axis.text = element_text(size = 12), axis.title = element_text(size = 15))
      
  })
  
  output$plot2 <- renderPlot({
    
    if (input$facet == "Shade") {
      ggplot(dataInput_2(), aes(x=Tsm, y= Hour)) + 
        xlab("Thermal Safety Margin (°C)") + geom_density_ridges2(aes(fill=Scenario), rel_min_height = 0.01, scale=2, alpha=0.5) + 
        ggtitle(paste(input$species_2,"|", names(month[as.numeric(input$month_2)]))) + 
        scale_y_discrete(limits = rev(levels(dataInput_2()$Hour)))  + theme_bw() + facet_wrap(~Shade) + theme(strip.text = element_text(size = 12)) +
        theme(plot.title = element_text(size = 18), axis.text = element_text(size = 12), axis.title = element_text(size = 14), legend.text = element_text(size = 12),
              legend.title = element_text(size = 12))
      
    } else {
      ggplot(dataInput_2(),aes(x=Tsm, y= Hour)) + 
        xlab("Thermal Safety Margin (°C)") + geom_density_ridges2(aes(fill=Shade),rel_min_height = 0.01 ,scale=2,alpha=0.5) + 
        ggtitle(paste(input$species_2,"|", names(month[as.numeric(input$month_2)]))) + 
        scale_y_discrete(limits = rev(levels(dataInput_2()$Hour)))  + theme_bw() + facet_wrap(~factor(Scenario, levels = scenarios)) + 
        scale_fill_manual(values = c("blue","red")) + theme(strip.text = element_text(size = 12)) +
        theme(plot.title = element_text(size = 18), axis.text = element_text(size = 12), axis.title = element_text(size = 14), legend.text = element_text(size = 12),
              legend.title = element_text(size = 12))
    }
    
  }, height = 600, width = 600)  
  
  
  data_by_org_data <- reactive({
    
    org <- input$species_data
    org <- "Coleonyx brevis"
    filename <- paste("Data\\",gsub(" ","_",org),"_combined.rds", sep= "")
    df <- readRDS(filename)
    df$Month <- factor(df$Month, levels = names(month))
    df$Hour <- factor(df$Hour, levels = hours)
    df$Scenario <- factor(df$Scenario, levels = scenarios)
    df
  })
  
  
  dataInput_data <- reactive({
    if(input$month_data == "Annual") {
      data_by_org_data() %>% filter(Scenario %in% input$scenario_data & Shade %in% input$shade_data)
      
    } else {
      data_by_org_data() %>% filter(Month %in%  names(month[as.numeric(input$month_data)]) & Scenario %in% input$scenario_data & Shade %in% input$shade_data)
    }
  })
  
  output$text_data <- renderText({
    str <- ""
    if(input$month_data == "Annual") {
      for(m in 1:12){
        count = 0
        for (hour in 1:length(hours)) {
          average <- mean(dataInput_data()[(dataInput_data()$Month == names(month[m])) & (dataInput_data()$Hour == hours[hour]), "Tsm"])
          if (average < input$value) {
            count = count + 1
            add <- paste(hours[hour], ": ", round(average, digits = 2), "°C")
            if (count == 1) {
              add <- HTML(paste('<b>', names(month)[m], '</b>', add, sep = '<br/>'))
            }
            str <- paste(str, add)
            str <- HTML(paste(str, "", sep = '<br/>'))
          }
        }
      }
    } else {
      for (hour in 1:length(hours)) {
        average <- mean(dataInput_data()[dataInput_data()$Hour == hours[hour], "Tsm"])
        if (average < input$value) {
          str <- paste(str, hours[hour], ": ", round(average, digits = 2), "°C")
          str <- HTML(paste(str, "", sep = '<br/>'))
        } 
      }
    }
    if(str == "") {
      str = "No data" 
    }
    if(input$month_data != "Annual") {
      str <- HTML(paste("<b>", names(month[as.numeric(input$month_data)]), "</b>", str, sep = '<br/>'))
    }
    str
  })
}

