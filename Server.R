month <- c(1:12)
names(month) <- c("January","February","March","April","May","June","July","August","September","October","November","December")
scenarios <- c("Normal","+1.5 °C","+2 °C")
hours <- c("12 AM","01 AM","02 AM","03 AM","04 AM","05 AM","06 AM","07 AM","08 AM","09 AM","10 AM","11 AM","12 PM","01 PM","02 PM","03 PM","04 PM","05 PM","06 PM","07 PM","08 PM","09 PM", "10 PM","11 PM")

#setwd("/Volumes/GoogleDrive/Shared Drives/TrEnCh/TSMVisualization/TSMdfs")

Lepidosauria <- fread("Lepidosauria.csv")


shinyServer <- function(input, output, session) {
  
  # Brief background information of selected species
  output$species_info <- renderText({
    org <- input$species
    filename <- paste("Lizards/",gsub(" ","_",org),".Rmd", sep= "")
    includeMarkdown(filename)
  })
  
  
  # Method 2: fread from csv
  # data_by_org <- eventReactive( input$run,{
  # 
  #   org <- input$species
  #   filename <- paste("Data/",gsub(" ","_",org),".csv", sep= "")
  #   df <- data.table::fread(filename)
  #   df$Month <- factor(df$Month, levels = names(month))
  #   df$Hour <- factor(df$Hour, levels = hours)
  #   df$Scenario <- factor(df$Scenario, levels = scenarios)
  #   df
  # }, ignoreNULL = FALSE
  # )
  
  # MAP
  #Method 3: RDS file
  #read the data file of the selected species to make a map
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
    data_by_org() %>% filter(Hour %in% input$hour & Month %in% input$month & Scenario %in% input$scenario & Shade %in% input$shade)
  }, ignoreNULL = FALSE
  )
  
  x_variable <- eventReactive( input$run, {
    input$columns
  }, ignoreNULL = FALSE
  )
  
  y_variable <- eventReactive( input$run, {
    input$rows
  }, ignoreNULL = FALSE
  )
  
  #Title of the map
  title <- eventReactive( input$run, {
    str <- input$species
    if (x_variable() != "Month" && y_variable() != "Month") {
      str <- paste(str, "|", input$month)
    }
    if (x_variable() != "Hour" && y_variable() != "Hour") {
      str <- paste(str, "|", input$hour)
    }
    if (x_variable() != "Scenario" && y_variable() != "Scenario") {
      str <- paste(str, "|", input$scenario)
    }
    if (x_variable() != "Shade" && y_variable() != "Shade") {
      str <- paste(str, "|", input$shade)
    }
    str
  }, ignoreNULL = FALSE
  )
  
  output$mymap <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = (max(dataInput()$x) + min(dataInput()$x))/2, lat = (max(dataInput()$y) + min(dataInput()$y))/2, zoom = 3) %>%
      addRectangles(lng1 = min(dataInput()$x), lng2 = max(dataInput()$x), lat1 = min(dataInput()$y), lat2 = max(dataInput()$y))
  })
  
  observeEvent(input$map_onoff,{
    toggle("mymap")
  })
  
  # width <- reactive({
  #   w <- max(dataInput()$x) - min(dataInput()$x)
  #   h <- max(dataInput()$y) - min(dataInput()$y)
  #   if (w < h) {
  #     400
  #   } else {
  #     350 * w / h  + 150
  #   }
  # })
  # 
  # height <- reactive({
  #   h <- max(dataInput()$y) - min(dataInput()$y)
  #   w <- max(dataInput()$x) - min(dataInput()$x)
  #   if (h < w) {
  #     400
  #   } else {
  #     250 * h / w + 50
  #   }
  # })
  
  output$plot1 <- renderPlot({    
    facet_formula <- as.formula(paste(x_variable(), "~", y_variable()))
    
    ggplot() +
      borders(fill="grey",colour="black") +
      ggtitle(title()) + xlab("Longitude (°)") + ylab("Latitude (°)") +
      geom_raster(data=dataInput(), aes(x = x, y = y, fill = cut(Tsm, c(-Inf, 0, 2, 5, Inf)))) + 
      scale_fill_manual(name = "Thermal Safety \nMargin (°C)", values = c("(-Inf,0]" = "red", "(0,2]" = "orange", "(2,5]" = "green", "(5, Inf]" = "blue"), 
                        labels = c("(-Inf,0]" = "< 0", "(0,2]" = "0 - 2", "(2,5]" = "2 - 5", "(5, Inf]" = "5 >")) +
      facet_grid(facet_formula) + coord_quickmap(xlim = c(min(dataInput()$x), max(dataInput()$x)), ylim = c(min(dataInput()$y), max(dataInput()$y)), expand = TRUE) +
      theme_bw( ) + theme(strip.text = element_text(size = 12)) + theme(plot.title = element_text(hjust = 0.5)) +
      #theme(plot.background = element_rect(fill = "#F5F5F5"), panel.background = element_rect(fill = "#F5F5F5")) +
      theme(plot.title = element_text(size = 18), axis.text = element_text(size = 12), axis.title = element_text(size = 14), legend.text = element_text(size = 12),
            legend.title = element_text(size = 12))
    
    
  }#, height = 600, width = 600
  )

    
  
  # observeEvent(input$plot_click, {
  #   output$plot1 <- renderPlot({
  #     facet_formula <- as.formula(paste(x_variable(), "~", y_variable()))
  #     
  #     ggplot() +
  #       borders(fill="grey",colour="black") +
  #       ggtitle(title()) + xlab("Longitude (°)") + ylab("Latitude (°)") +
  #       geom_raster(data=dataInput(), aes(x = x, y = y, fill = cut(Tsm, c(-Inf, 0, 2, 5, Inf)))) + 
  #       scale_fill_manual(name = "Thermal Safety \nMargin (°C)", values = c("(-Inf,0]" = "red", "(0,2]" = "orange", "(2,5]" = "green", "(5, Inf]" = "blue"), 
  #                         labels = c("(-Inf,0]" = "< 0", "(0,2]" = "0 - 2", "(2,5]" = "2 - 5", "(5, Inf]" = "5 >")) +
  #       facet_grid(facet_formula) + coord_quickmap(xlim = c(min(dataInput()$x), max(dataInput()$x)), ylim = c(min(dataInput()$y), max(dataInput()$y)),expand = TRUE) +
  #       theme_bw( ) + theme(strip.text = element_text(size = 12)) + 
  #       theme(plot.background = element_rect(fill = "#F5F5F5"), panel.background = element_rect(fill = "#F5F5F5")) +
  #       theme(plot.title = element_text(size = 18), axis.text = element_text(size = 12), axis.title = element_text(size = 14), legend.text = element_text(size = 12),
  #             legend.title = element_text(size = 12)) + 
  #     geom_label(x = input$plot_click[1], y = input$plot_click[2], label = paste("TSM:", round(colMeans(nearPoints(dataInput(), input$plot_click, xvar = "x", yvar = "y")["Tsm"]), digits = 1), "°C"))
  #       
  #   })
  #       
  # })
  
  step <- eventReactive(input$run,{
    n = 1
    if (x_variable() == "Month" | y_variable() == "Month") {
      n = n * length(input$month)
    } 
    if (x_variable() == "Hour" | y_variable() == "Hour") {
      n = n * length(input$hour)
    } 
    if (x_variable() == "Scenario" | y_variable() == "Scenario") {
      n = n * length(input$scenario)
    } 
    if (x_variable() == "Shade" | y_variable() == "Shade"){
      n = n * length(input$shade)
    }
    n
  }, ignoreNULL = FALSE)


  output$density <- renderPlot({
    
    facet_formula <- as.formula(paste(x_variable(), "~", y_variable()))
    mx <- max(dataInput()$Tsm)
    mn <- min(dataInput()$Tsm)
    p <- ggplot(data = dataInput()) + 
      geom_density_ridges2(aes(x=Tsm, y=0, fill=Scenario), rel_min_height = 0.01, scale=2, alpha=0.5) +
      xlab("Thermal Safety Margin (°C)") + ylab("Fraction") + scale_fill_manual(name = "Scenarios", values = c("tan", "tan2", "tan4"), labels = c("Normal", "+ 1.5°C", "+ 2°C")) +
      #annotate("rect", xmin = 0, xmax = Inf, ymin = 0, ymax = Inf, alpha = 0.2, fill = "red") +
      scale_x_continuous(expand = c(0,0)) + scale_y_continuous(expand = c(0,0)) +
      theme_bw() + facet_grid(facet_formula, scales = "free_x") + theme(strip.text = element_text(size = 12)) + theme(legend.position = "none") +
      theme(legend.position = "right") +
      theme(plot.title = element_text(size = 18), axis.text = element_text(size = 12), axis.title = element_text(size = 14), plot.background = element_rect(fill = "#F5F5F5"), 
            panel.background = element_rect(fill = "azure"), legend.text = element_text(size = 12), legend.title = element_text(size = 12))
    
    
               
    if (mn < 5) {
      p <- p + geom_vline(xintercept = -0.1, size = 1.2, col = "red") + geom_segment(aes(x = -Inf, y = 0, xend = -0.1, yend = 0), col = "red", size = 1.2)
    }
    if(mx > -8 & mn < 7) {
      p <- p + geom_vline(xintercept = c(0.1, 1.9), size = 1.2, col = "orange") + geom_segment(aes(x = 0.1, y = 0, xend = 1.9, yend = 0), col = "orange", size = 1.2)
    }
    if (mx > 0 & mn < 10) {
      p <- p + geom_vline(xintercept = c(2.1, 4.9), size = 1.2, col = "green") + geom_segment(aes(x = 2.1, y = 0, xend = 4.9, yend = 0), col = "green", size = 1.2)
    }
    if (mx > 0) {
      p <- p + geom_vline(xintercept = 5.1, size = 1.2, col = "blue") + geom_segment(aes(x = 5.1, y = 0, xend = Inf, yend = 0), col = "blue", size = 1.2)
    }
    p
  })
  
  output$info <- renderText({
    tsm <- round(colMeans(nearPoints(dataInput(), input$plot_click, xvar = "x", yvar = "y")["Tsm"]), digits = 1)
    tmax <- Lepidosauria[Lepidosauria$Binomial == input$species, "Tmax"]
    output <- paste("Operative temperature:", tmax - tsm, "°C\nTSM:", tsm, "°C")
    output
  })
  
  
  # TPC
  dataTPC <- reactive({
    org <- input$species
    filename <- paste("Data/",gsub(" ","_",org),"_combined.rds", sep= "")
    df <- readRDS(filename) %>% filter(Hour %in% input$hour_tpc & Month %in% names(month[as.numeric(input$month_tpc)]) & Scenario %in% input$scenario_tpc & Shade %in% input$shade_tpc)
    Lepidosauria[Lepidosauria$Binomial == input$species, Tmax] - mean(df$Tsm)
  })
  
  output$TPC <- renderPlot({
    Lepidosauria <- Lepidosauria[Lepidosauria$Binomial == input$species, ]
    curve <- TPC(0:pmax(50, dataTPC()), Lepidosauria$Topt, as.numeric(as.character(Lepidosauria$tmin)), Lepidosauria$Tmax)
    
    ggplot() + geom_line(data = NULL, aes(x = c(0:pmax(50, dataTPC())), y = curve), col = "steelblue", size = 1.2) + xlab("Temperature (°C)") + ylab("Performance") +
      geom_segment(aes(x = Lepidosauria$Tmax - 5, y = 0.1, xend = Lepidosauria$Tmax, yend = 0), size = 1) + geom_text(aes(x = Lepidosauria$Tmax - 6, y = 0.14, label = "Tmax"), size = 5) +
      geom_vline(xintercept = dataTPC(), size = 1.2) + geom_text(aes(x = dataTPC() - 1.7, label = "Operative temperature", y = 0.45), size = 5, angle = 90) + theme_classic( ) +
      geom_rect(aes(xmin = Lepidosauria$Tmax, xmax = Inf, ymin = -Inf, ymax = Inf), alpha = 0.7, fill = "red") +
      scale_x_continuous(limits = c(0,pmax(50, dataTPC())+2), expand = c(0,0)) + scale_y_continuous(limits = c(0, 1.05), expand = c(0,0)) +
      theme(axis.text = element_text(size = 12), axis.title = element_text(size = 15), plot.background = element_rect(fill = "#F5F5F5"), 
            panel.background = element_rect(fill = "azure"))
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
              legend.title = element_text(size = 12), plot.background = element_rect(fill = "#F5F5F5"), panel.background = element_rect(fill = "azure"))
    } else {
      ggplot(dataInput_2(),aes(x=Tsm, y= Hour)) + 
        xlab("Thermal Safety Margin (°C)") + geom_density_ridges2(aes(fill=Shade),rel_min_height = 0.01 ,scale=2,alpha=0.5) + 
        ggtitle(paste(input$species,"|", names(month[as.numeric(input$month_2)]))) + 
        scale_y_discrete(limits = rev(levels(dataInput_2()$Hour)))  + theme_bw() + facet_wrap(~factor(Scenario, levels = scenarios)) + 
        scale_fill_manual(values = c("blue","red")) + theme(strip.text = element_text(size = 12)) +
        theme(plot.title = element_text(size = 18), axis.text = element_text(size = 12), axis.title = element_text(size = 14), legend.text = element_text(size = 12),
              legend.title = element_text(size = 12), plot.background = element_rect(fill = "#F5F5F5"), panel.background = element_rect(fill = "azure"))
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

