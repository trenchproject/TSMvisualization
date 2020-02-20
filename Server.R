month <- c(1:12)
names(month) <- c("January","February","March","April","May","June","July","August","September","October","November","December")
scenarios <- c("Normal","+1.5 °C","+2 °C")
hours <- c("12 AM","01 AM","02 AM","03 AM","04 AM","05 AM","06 AM","07 AM","08 AM","09 AM","10 AM","11 AM","12 PM","01 PM","02 PM","03 PM","04 PM","05 PM","06 PM","07 PM","08 PM","09 PM", "10 PM","11 PM")


shinyServer <- function(input, output) {
  
  data_by_org <- reactive({
    org <- input$select_species
    spdata <- paste("TSMdfs\\",gsub(" ","_",org),"_combined.Rda", sep= "")
    load(spdata)
    combined
  })
  
  dataInput <- reactive({
    data_by_org() %>% filter(Hour %in% input$select_hour & Month %in% names(month[as.numeric(input$select_month)]) & Scenario %in% input$select_scenario)
  })
  
  dataInput_2 <- reactive({
    data_by_org() %>% filter(Month %in% names(month[as.numeric(input$select_month_2)]))
  })
  
  x_variable <- reactive({
    switch(input$columns, Month = "Month", Hour = "Hour", Scenario = "Scenario", Shade = "Shade")
  })
  
  y_variable <- reactive({
    switch(input$rows, Month = "Month", Hour = "Hour", Scenario = "Scenario", Shade = "Shade")
  })
  
  title <- reactive({
    title_m <- names(month[as.numeric(input$select_month)])
    title_h <- input$select_hour
    title_s <- input$select_scenario
    
    if (as.character(x_variable()) == "Month" | as.character(y_variable()) == "Month") {
      title_m <- ""
    } 
    if (as.character(x_variable()) == "Hour" | as.character(y_variable()) == "Hour") {
      title_h <- ""
    } 
    if (as.character(x_variable()) == "Scenario" | as.character(y_variable()) == "Scenario") {
      title_s <- ""
    } 
    paste(org, "|", title_m, title_h, title_s)
  })
  
  output$plot1 <- renderPlot({
    
    facet_formula_x <- as.formula(paste(x_variable(), "~", y_variable()))
    
    ggplot() +
      borders(fill="grey",colour="black") +
      ggtitle(title()) + xlab("Longtitude") + ylab("Latitude") +
      geom_raster(data=dataInput(), aes(x = x, y = y, fill = Tsm) , interpolate = TRUE) + scale_fill_gradient(low = "red", high = "blue", limits = c(-60, 60), name = "Thermal Safety Margin") +
      facet_grid(facet_formula_x) +
      coord_quickmap(xlim = c(min(dataInput()$x), max(dataInput()$x)), ylim = c(min(dataInput()$y), max(dataInput()$y)),expand = TRUE) +
      theme_bw( ) + theme(strip.text = element_text(size = 12)) +
      theme(plot.title = element_text(size = 16), axis.text = element_text(size = 12), axis.title = element_text(size = 14), legend.text = element_text(size = 12), legend.title = element_text(size = 12))
  }, height = 600, width = 800)
  
  
  output$plot2 <- renderPlot({
    
    ggplot(dataInput_2(), aes(x=Tsm, y= Hour)) + xlab("Thermal Safety Margin") +
      geom_density_ridges2(aes(fill=Scenario), rel_min_height = 0.01, scale=2, alpha=0.5) + ggtitle(paste(org,"|", names(month[as.numeric(input$select_month_2)]))) + 
      scale_y_discrete(limits = rev(levels(dataInput_2()$Hour)))  + theme_bw() + facet_wrap(~Shade) + theme(strip.text = element_text(size = 12)) +
      theme(plot.title = element_text(size = 16), axis.text = element_text(size = 12), axis.title = element_text(size = 14), legend.text = element_text(size = 12), legend.title = element_text(size = 12))
    
  }, height = 600, width = 800)  
  
}