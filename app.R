library(rjson)
library(httr)
library(tidyverse)
library(plotly)
library(shiny)
library(shinyWidgets)


ui <- shiny::fluidPage(
  titlePanel("North Atlantic Sea Surface Temperature"),
  p("Difference from 1981-2023 mean/median"),
  sidebarLayout(
    sidebarPanel(
      HTML("<b>Highlight a year by clicking a band</b>"),
      plotlyOutput("tile", height = "100px"),
      shinyWidgets::radioGroupButtons("aggregation", "Difference", choices = c("Mean", "Median"), selected = "Mean"), 
      shinyWidgets::radioGroupButtons("color_choice", "Color", choices = c("Grey", "RedBlue (Year mean)"), selected = "Grey"), 
      actionButton("refresh_data",label = "Refresh", icon = icon("arrows-rotate")),
      p(),
      htmlOutput("help_text")
      
    ),
    mainPanel(
      fluidRow(
        column(12, align="center",
               plotlyOutput("plot", height = "600px")
        )
      )
    )))



server <- function(input, output, session){
  
  RV <- reactiveValues()
  
  observe({
    invalidateLater(4.32e+7)
    RV$query_date <- Sys.Date()
    api_request <- "https://climatereanalyzer.org/clim/sst_daily/json/oisst2.1_natlan1_sst_day.json"
    product_data <- content(httr::GET(api_request),as = "parsed", type = "application/json")
    
    # get names
    df_names <- product_data %>% map_chr("name")
    # get data
    RV$df_temp <- product_data[1:43] %>% 
      map("data") %>% 
      map(~map(.,~ifelse(is.null(.), NA, .))) %>% 
      map(~tibble(temp=unlist(.))) %>%
      map(~mutate(.,day= 1:n())) %>% 
      set_names(df_names[1:43]) %>% 
      bind_rows(.id = "year")
    
    
  })
  
  
  
  observeEvent(input$refresh_data,{
    
    RV$query_date <- Sys.Date()
    api_request <- "https://climatereanalyzer.org/clim/sst_daily/json/oisst2.1_natlan1_sst_day.json"
    product_data <- content(httr::GET(api_request),as = "parsed", type = "application/json")
    
    # get names
    df_names <- product_data %>% map_chr("name")
    # get data
    RV$df_temp <- product_data[1:43] %>% 
      map("data") %>% 
      map(~map(.,~ifelse(is.null(.), NA, .))) %>% 
      map(~tibble(temp=unlist(.))) %>%
      map(~mutate(.,day= 1:n())) %>% 
      set_names(df_names[1:43]) %>% 
      bind_rows(.id = "year")
    
    
    showNotification("Data successfully fetched", type ="message")
    
  })
  
  
  
  
  
  
  output$plot <- renderPlotly({
    
    req(RV$df_temp)
    RV$df_year_diff <-  RV$df_temp %>% 
      group_by(year) %>% 
      summarise(mean_year = mean(temp, na.rm = T),
                n = sum(is.na(temp), na.rm=T)) %>%
      mutate(diff_year = mean_year-mean(mean_year)) %>% 
      mutate(diff_year = ifelse(n>100, 0, diff_year))
    
    year_selected  = "2023"
    
    d <- event_data("plotly_click", source = "tile")
    if(!is.null(d$x)){
      year_selected <- RV$df_year_diff$year[d$x]
    }
    
    RV$df_mean <-  switch (input$aggregation,
                           Mean = RV$df_temp %>% 
                             # filter(year %in% 1983:2011) %>%
                             group_by(day) %>% 
                             summarise(mean_temp = mean(temp, na.rm = T)),
                           Median = RV$df_temp %>% 
                             group_by(day) %>% 
                             summarise(mean_temp = median(temp, na.rm = T)))
    
    tmp_data <-  RV$df_temp %>% 
      left_join(RV$df_mean,by = "day") %>% 
      mutate(diff = temp-mean_temp)
    
    
    
    if(input$color_choice == "Grey"){
      
      
      g <- tmp_data %>% 
        ggplot(aes(day, diff, group = year,  text = paste0("Year: ", year,
                                                           "\nDay: ", day," / ",as.Date(day-1, origin = paste0(year,"-01-01")),
                                                           "\nTemperature [°C]: ",round(temp,2),
                                                           "\nDifference [°C]: ", round(diff,2)))) + 
        geom_hline(yintercept = 0, linetype = 2)+
        geom_line(data = . %>% filter(year != year_selected), show.legend = F, color = "grey") + 
        geom_line(data = . %>% filter(year == year_selected), size  = 1.08, color = "deeppink") +  
        # scale_color_manual(values = c("grey", "deeppink"))+
        # ylim(-1.1,1.1) + 
        ylab(paste(input$aggregation,"Difference [°C]"))+ xlab("Day")+
        geom_vline(xintercept = lubridate::yday(Sys.Date())) + 
        theme_bw()
      
      
    }else{
      
      
      
      g <- tmp_data %>% 
        left_join(RV$df_year_diff, by = "year") %>% 
        ggplot(aes(day, diff, group = year, color=diff_year, text = paste0("Year: ", year,
                                                                           "\nDay: ", day," / ",as.Date(day-1, origin = paste0(year,"-01-01")),
                                                                           "\nTemperature [°C]: ",round(temp,2),
                                                                           "\nDifference [°C]: ", round(diff,2)))) + 
        geom_hline(yintercept = 0, linetype = 2)+
        geom_line(data = . %>% filter(year != year_selected), show.legend = F) + 
        geom_line(data = . %>% filter(year == year_selected), size  = 1.08, color = "deeppink") +  
        scale_colour_gradient2(low = "darkblue",mid = "lightblue",high = "red", midpoint = 0)+
        # ylim(-1.1,1.1) + 
        ylab(paste(input$aggregation,"Difference [°C]"))+ xlab("Day")+
        geom_vline(xintercept = lubridate::yday(Sys.Date())) + 
        theme_bw()
      
    }
    ggplotly(g, tooltip="text") %>% 
      layout(showlegend = F)
    
    
  })
  
  
  output$tile <- renderPlotly({
    
    req(RV$df_temp)
    
    g <- ggplot(RV$df_year_diff,
                aes(x = year, y = 1, fill = diff_year, text  = paste0("Year: ", year,
                                                                      "\nMean difference: ", round(diff_year,2)))) + 
      geom_tile(show.legend = F)+
      scale_fill_gradient2("",low = "darkblue",mid = "lightblue",high = "red", midpoint = 0, 
                           na.value = "lightgrey")+
      theme_void() 
    
    ggplotly(g, tooltip = "text", source = "tile") %>% 
      layout(showlegend = F,
             paper_bgcolor='rgba(0,0,0,0)',
             plot_bgcolor='rgba(0,0,0,0)',
             xaxis= list(showticklabels = FALSE,fixedrange = TRUE, showline = F,showgrid =F),
             yaxis= list(showticklabels = FALSE,fixedrange = TRUE, showline = F, showgrid =F)) %>% 
      event_register("plotly_click") %>% 
      config(displayModeBar = FALSE)
    
    
  })
  
  
  output$help_text <- renderUI({
    req(RV$query_date)
    HTML(paste("Data fetched on", RV$query_date, 
               "from <a href='https://climatereanalyzer.org/clim/sst_daily/json/oisst2.1_natlan1_sst_day.json'>NOAA Optimum Interpolation SSt (OISST).</a>"))
  })
  
  
}
shinyApp(ui, server)


