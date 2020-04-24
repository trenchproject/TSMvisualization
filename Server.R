month <- c(1:12)
names(month) <- c("January","February","March","April","May","June","July","August","September","October","November","December")
scenarios <- c("Normal","+1.5 °C","+2 °C")
hours <- c("12 AM","01 AM","02 AM","03 AM","04 AM","05 AM","06 AM","07 AM","08 AM","09 AM","10 AM","11 AM","12 PM","01 PM","02 PM","03 PM","04 PM","05 PM","06 PM","07 PM","08 PM","09 PM", "10 PM","11 PM")
TPC<- function(T,Topt,CTmin, CTmax){
  F=T
  F[]=NA
  sigma= (Topt-CTmin)/4
  F[T<=Topt & !is.na(T)]= exp(-((T[T<=Topt & !is.na(T)]-Topt)/(2*sigma))^2) 
  F[T>Topt & !is.na(T)]= 1- ((T[T>Topt & !is.na(T)]-Topt)/(Topt-CTmax))^2
  #set negetative to zero
  F[F<0]<-0
  
  return(F)
}

#setwd("/Volumes/GoogleDrive/Shared Drives/TrEnCh/TSMVisualization/TSMdfs")

lizards_tpc <- fread("lizards_tpc.csv")


shinyServer <- function(input, output, session) {
  
  # INTRO
  output$introduction <- renderText ({
    "Thermal safety margin is the difference between the operative temperature and the critical thermal maximum of a given organism. <br>
    Here we define critical thermal maximum as the highest temperature the organism can survive and reproduce. In other words, TSM indicates how much more the temperature can increase before the environment
    turns unfeasible for them to live.  <br>
    <br>
    A positive TSM means the environmental temperature is within the thermal tolerance of the organism and a negative TSM implies that the operative temperature surpasses the critical thermal maximum.
    TSM is a good indicator of whether the environment is suitable for certain organisms, and it has potential for predicting whether species will be able to keep
    surving in the same habitat in the world of accelerating climate change. <br>
    <br>
    In this platform, we use TSM to visualize and address the danger some species might face with increased temperature."
  })
  
  
  # Introduction figure
  output$intro_fig <- renderPlot({
    
    curve <- TPC(0:50, 35.6, 8.8, 45.5)
    
    ggplot() + geom_line(data = NULL, aes(x = c(0:50), y = curve), col = "steelblue", size = 1.1) + xlab("Temperature (°C)") + ylab("Performance") +
      geom_segment(aes(x = 40.5, y = 0.1, xend = 45.5, yend = 0), size = 1) + geom_text(aes(x = 45.5 - 6, y = 0.14, label = "Tmax"), size = 5) +
      geom_segment(aes(x = 33, y = 0.72, xend = 45.5, yend = 0.72), arrow = arrow(length = unit(0.4, "cm"), ends = "both"), size = 1) +
      geom_text(aes(x = 15, y = 0.9, label = "Thermal Safety margin"), size = 6.5) + geom_segment(aes(x = 26, y = 0.88, xend = 39, yend = 0.73), size = 0.8, arrow = arrow(length = unit(0.3, "cm"))) +
      geom_vline(xintercept = 33, size = 1.2, lty = 5, col = "blue") + geom_text(aes(x = 33 - 1.7, label = "Operative temperature", y = 0.4), size = 5, angle = 90, col = "blue") + theme_classic( ) +
      geom_rect(aes(xmin = 45.5, xmax = Inf, ymin = -Inf, ymax = Inf), alpha = 0.7, fill = "red") +
      scale_x_continuous(limits = c(0, 50 + 2), expand = c(0,0)) + scale_y_continuous(limits = c(0, 1.05), expand = c(0,0)) +
      theme(axis.text = element_text(size = 12), axis.title = element_text(size = 15))
  }, height = 350, width = 500)
  
  # Brief background information of selected species
  output$species_info <- renderText({
    org <- input$species
    filename <- paste("Lizards/",gsub(" ","_",org),".Rmd", sep= "")
    includeMarkdown(filename)
    
  })
  
  
  #Method 2: fread from csv
  # data_by_org <- eventReactive( input$run,{
  # 
  #   org <- input$species_1
  #   filename <- paste("Data/",gsub(" ","_",org),"_combined.csv", sep= "")
  #   df <- data.table::fread(filename)
  #   df$Month <- factor(df$Month, levels = names(month))
  #   df$Hour <- factor(df$Hour, levels = hours)
  #   df$Scenario <- factor(df$Scenario, levels = scenarios)
  #   df
  # }, ignoreNULL = FALSE
  # )
  
  # MAP
  #Method 3: RDS file
  # read the data file of the selected species to make a map
  data_by_org <- eventReactive( input$run,{
    
    org <- input$species
    filename <- paste("Data/",gsub(" ","_",org),"_combined.rds", sep= "")
    df <- readRDS(filename)
    df$Month <- factor(df$Month, levels = names(month))
    df$Hour <- factor(df$Hour, levels = hours)
    df$Scenario <- factor(df$Scenario, levels = scenarios)
    df
  }, ignoreNULL = FALSE
  )
  
  # Filter data by selected inputs to map the distribution
  dataInput <- eventReactive( input$run,{
    data_by_org() %>% filter(Hour %in% input$hour & Month %in% names(month[as.numeric(input$month)]) & Scenario %in% input$scenario & Shade %in% input$shade)
  }, ignoreNULL = FALSE
  )
  
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
    paste(input$species, title_m, title_h, title_sc, title_sh)
  }, ignoreNULL = FALSE
  )
  
  output$mymap <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$OpenStreetMap) %>%
      setView(lng = (max(dataInput()$x) + min(dataInput()$x))/2,  lat = (max(dataInput()$y) + min(dataInput()$y))/2, zoom = 3) %>%
      addRectangles(lng1 = min(dataInput()$x), lng2 = max(dataInput()$x), lat1 = min(dataInput()$y), lat2 = max(dataInput()$y))
  })
  
  observeEvent(input$map_onoff,{
    toggle("mymap")
  }, ignoreNULL = FALSE
  )
  
  output$plot1 <- renderPlot({
    
    facet_formula <- as.formula(paste(x_variable(), "~", y_variable()))
    
    ggplot() +
      borders(fill="grey",colour="black") +
      ggtitle(title()) + xlab("Longitude (°)") + ylab("Latitude (°)") +
      geom_raster(data=dataInput(), aes(x = x, y = y, fill = cut(Tsm, c(-Inf, 0, 2, 5, Inf)))) + 
      scale_fill_manual(name = "Thermal Safety \nMargin (°C)", values = c("(-Inf,0]" = "red", "(0,2]" = "orange", "(2,5]" = "green", "(5, Inf]" = "blue"), 
                        labels = c("(-Inf,0]" = "< 0", "(0,2]" = "0 - 2", "(2,5]" = "2 - 5", "(5, Inf]" = "5 >")) +
      facet_grid(facet_formula) + coord_quickmap(xlim = c(min(dataInput()$x), max(dataInput()$x)), ylim = c(min(dataInput()$y), max(dataInput()$y)),expand = TRUE) +
      theme_bw( ) + theme(strip.text = element_text(size = 12)) +
      theme(plot.title = element_text(size = 18), axis.text = element_text(size = 12), axis.title = element_text(size = 14), legend.text = element_text(size = 12),
            legend.title = element_text(size = 12))
  }, height = 650, width = 650)
  
  output$density <- renderPlot({
    
    facet_formula <- as.formula(paste(x_variable(), "~", y_variable()))
    
    ggplot(dataInput(), aes(x=cut(Tsm, c(-Inf, 0, 2, 5, Inf)), fill = cut(Tsm, c(-Inf, 0, 2, 5, Inf)))) + 
      geom_bar(aes(y = (..count..)/sum(..count..)*100)) + scale_x_discrete(labels = c("(-Inf,0]" = "< 0", "(0,2]" = "0 - 2", "(2,5]" = "2 - 5", "(5, Inf]" = "5 >")) +
      xlab("Thermal Safety Margin (°C)") + ylab("Percentage") + ggtitle(title()) + 
      scale_fill_manual(values = c("(-Inf,0]" = "red", "(0,2]" = "orange", "(2,5]" = "green", "(5, Inf]" = "blue")) +
      theme_bw() + facet_grid(facet_formula) + theme(strip.text = element_text(size = 12)) + theme(legend.position = "none") +
      theme(plot.title = element_text(size = 18), axis.text = element_text(size = 12), axis.title = element_text(size = 14))
  })
  
  
  # TPC
  dataTPC <- reactive({
    org <- input$species
    filename <- paste("Data/",gsub(" ","_",org),"_combined.rds", sep= "")
    df <- readRDS(filename) %>% filter(Hour %in% input$hour_tpc & Month %in% names(month[as.numeric(input$month_tpc)]) & Scenario %in% input$scenario_tpc & Shade %in% input$shade_tpc)
    lizards_tpc[lizards_tpc$Binomial == input$species, Tmax] - mean(df$Tsm)
  })
  
  output$TPC <- renderPlot({
    lizards_tpc <- lizards_tpc[lizards_tpc$Binomial == input$species, ]
    curve <- TPC(0:pmax(50, dataTPC()), lizards_tpc$Topt, as.numeric(as.character(lizards_tpc$tmin)), lizards_tpc$Tmax)
    
    ggplot() + geom_line(data = NULL, aes(x = c(0:pmax(50, dataTPC())), y = curve), col = "steelblue", size = 1.2) + xlab("Temperature (°C)") + ylab("Performance") +
      geom_segment(aes(x = lizards_tpc$Tmax - 5, y = 0.1, xend = lizards_tpc$Tmax, yend = 0), size = 1) + geom_text(aes(x = lizards_tpc$Tmax - 6, y = 0.14, label = "Tmax"), size = 5) +
      geom_vline(xintercept = dataTPC(), size = 1.2) + geom_text(aes(x = dataTPC() - 1.7, label = "Operative temperature", y = 0.45), size = 5, angle = 90) + theme_classic( ) +
      geom_rect(aes(xmin = lizards_tpc$Tmax, xmax = Inf, ymin = -Inf, ymax = Inf), alpha = 0.7, fill = "red") +
      scale_x_continuous(limits = c(0,pmax(50, dataTPC())+2), expand = c(0,0)) + scale_y_continuous(limits = c(0, 1.05), expand = c(0,0)) +
      theme(axis.text = element_text(size = 12), axis.title = element_text(size = 15))
  })
  
  # PLOT  
  # data_by_org_2 <- reactive({
  #   org_2 <- input$species_2
  #   filename <- paste("Data/",gsub(" ","_",org_2),"_combined.csv", sep= "")
  #   df <- data.table::fread(filename)
  #   levels(df$Hour) <- hours
  #   levels(df$Month) <- month
  #   df
  # })
  
  # read the data file of the selected species to make a plot
  data_by_org_2 <- reactive({
    org_2 <- input$species
    filename <- paste("Data/",gsub(" ","_",org_2),"_combined.rds", sep= "")
    df2 <- readRDS(filename)
    # levels(df2$Hour) <- hours
    # levels(df2$Month) <- month
    df2$Month <- factor(df2$Month, levels = names(month))
    df2$Hour <- factor(df2$Hour, levels = hours)
    df2$Scenario <- factor(df2$Scenario, levels = scenarios)
    df2
  })
  
  # Filter data by selected inputs to plot density
  dataInput_2 <- reactive({
    data_by_org_2() %>% filter(Month %in% names(month[as.numeric(input$month_2)]))
  })
  
  output$plot2 <- renderPlot({
    
    if (input$facet == "Shade") {
      ggplot(dataInput_2(), aes(x=Tsm, y= Hour)) + 
        xlab("Thermal Safety Margin (°C)") + geom_density_ridges2(aes(fill=Scenario), rel_min_height = 0.01, scale=2, alpha=0.5) + 
        ggtitle(paste(input$species,"|", names(month[as.numeric(input$month_2)]))) + 
        scale_y_discrete(limits = rev(levels(dataInput_2()$Hour)))  + theme_bw() + facet_wrap(~Shade) + theme(strip.text = element_text(size = 12)) +
        theme(plot.title = element_text(size = 18), axis.text = element_text(size = 12), axis.title = element_text(size = 14), legend.text = element_text(size = 12),
              legend.title = element_text(size = 12))
    } else {
      ggplot(dataInput_2(),aes(x=Tsm, y= Hour)) + 
        xlab("Thermal Safety Margin (°C)") + geom_density_ridges2(aes(fill=Shade),rel_min_height = 0.01 ,scale=2,alpha=0.5) + 
        ggtitle(paste(input$species,"|", names(month[as.numeric(input$month_2)]))) + 
        scale_y_discrete(limits = rev(levels(dataInput_2()$Hour)))  + theme_bw() + facet_wrap(~factor(Scenario, levels = scenarios)) + 
        scale_fill_manual(values = c("blue","red")) + theme(strip.text = element_text(size = 12)) +
        theme(plot.title = element_text(size = 18), axis.text = element_text(size = 12), axis.title = element_text(size = 14), legend.text = element_text(size = 12),
              legend.title = element_text(size = 12))
    }
  }, height = 600, width = 600)  
  
  # DATA
  data_by_org_data <- reactive({
    org <- input$species
    filename <- paste("Data/",gsub(" ","_",org),"_combined.rds", sep= "")
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
      data_by_org_data() %>% filter(Month %in% names(month[as.numeric(input$month_data)]) & Scenario %in% input$scenario_data & Shade %in% input$shade_data)
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

