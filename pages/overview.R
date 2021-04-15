library(tidyverse)
library(plotly)
library(lubridate)

overview_page.ui <- {
  fluidPage(
    fluidRow(
      h2("Overview")
    ),
    fluidRow(
      column(6, dateInput("overview_page.ui.select.date", "Select a day:"),),
      column(6, selectInput("overview_page.ui.select.frequency", 
                            label = "Select frequency:",
                            choices = c("daily", "weekly", "monthly", "yearly")))
    ),
    fluidRow(
      infoBoxOutput("overview_page.ui.num_costumers"),
      infoBoxOutput("overview_page.ui.services_income"),
      infoBoxOutput("overview_page.ui.services_mean_income"),
      infoBoxOutput("overview_page.ui.num_services"),
      infoBoxOutput("overview_page.ui.num_products"),
      infoBoxOutput("overview_page.ui.products_income"),
      infoBoxOutput("overview_page.ui.total_income"),
    ),
    fluidRow(
      tabBox(
        width = 12, 
        tabPanel("Services", 
                 fluidRow(
                   column(width=6, plotlyOutput(outputId = "overview_page.ui.most_used.services.plot")),
                   column(width=6, plotlyOutput(outputId = "overview_page.ui.most_profitable.services.plot"))
                 )
        ),
        tabPanel("Products", 
                 fluidRow(
                   column(width=6, plotlyOutput(outputId = "overview_page.ui.most_used.products.plot")),
                   column(width=6, plotlyOutput(outputId = "overview_page.ui.most_profitable.products.plot"))
                 )
        )
      )
    )
  )
}

overview_page.server <- function(input, output, session) {

  # information about the selected costumer
  service_stats <- reactive({
    freq <- input$overview_page.ui.select.frequency
    date <- input$overview_page.ui.select.date
    
    helper.stats_service(date, freq)
  })
  
  most.service_stats <- reactive({
    freq <- input$overview_page.ui.select.frequency
    date <- input$overview_page.ui.select.date

    helper.most.stats_service(date, freq)
  })
  
  product_stats <- reactive({
    freq <- input$overview_page.ui.select.frequency
    date <- input$overview_page.ui.select.date
    
    helper.stats_product(date, freq)
  })
  
  most.product_stats <- reactive({
    freq <- input$overview_page.ui.select.frequency
    date <- input$overview_page.ui.select.date
    
    helper.most.stats_product(date, freq)
  })
  
  # PLOT
  
  # most used 
  output$overview_page.ui.most_used.services.plot <- renderPlotly({
    stats <- most.service_stats()
    
    data_to_plot <- stats %>%
      select(service_name, most_used) %>%
      arrange(desc(most_used))
       
     plot_ly(data_to_plot, x = ~service_name, y = ~most_used, type = 'bar',
             text = ~most_used, textposition = 'auto') %>% 
       layout(title = "Most used services", 
              xaxis = list(title="", tickangle = -45, categoryorder = "array", categoryarray = ~most_used),
              yaxis = list(title="Frequency"))
  })
  # most profitable
  output$overview_page.ui.most_profitable.services.plot <- renderPlotly({
    stats <- most.service_stats()
    
    data_to_plot <- stats %>%
      select(service_name, most_profitable) %>%
      arrange(desc(most_profitable))
    
    plot_ly(data_to_plot, x = ~service_name, y = ~most_profitable, type = 'bar',
            text = ~round(most_profitable, 2), textposition = 'auto') %>% 
      layout(title = "Most profitable services", 
             xaxis = list(title="", tickangle = -45, categoryorder = "array", categoryarray = ~most_profitable),
             yaxis = list(title="Income €"))
  })
  
  # most used 
  output$overview_page.ui.most_used.products.plot <- renderPlotly({
    stats <- most.product_stats()
    
    data_to_plot <- stats %>%
      select(product_name, most_used) %>%
      slice_max(most_used, n=10)
    
    plot_ly(data_to_plot, x = ~product_name, y = ~most_used, type = 'bar',
            text = ~most_used, textposition = 'auto') %>% 
      layout(title = "Most sold products", 
             xaxis = list(title="", tickangle = -45, categoryorder = "array", categoryarray = ~most_used),
             yaxis = list(title="Frequency"))
  })
  
  # most profitable
  output$overview_page.ui.most_profitable.products.plot <- renderPlotly({
    
    stats <- most.product_stats()
    
    data_to_plot <- stats %>%
      select(product_name, most_profitable) %>%
      slice_max(most_profitable, n=10)
    
    plot_ly(data_to_plot, x = ~product_name, y = ~most_profitable, type = 'bar',
            text = ~round(most_profitable, 2), textposition = 'auto') %>% 
      layout(title = "Most profitable prodcuts", 
             xaxis = list(title="", tickangle = -45, categoryorder = "array", categoryarray = ~most_profitable),
             yaxis = list(title="Income €"))
  })
  
  
  # info box
  output$overview_page.ui.num_costumers <- renderInfoBox({
    stats <- service_stats()
    
    unique_client <- stats["unique_client"][[1]]
    infoBox("N. costumers", unique_client, icon = icon("user"), fill = FALSE, color = "red")
  })
  output$overview_page.ui.services_income <- renderInfoBox({
    stats <- service_stats()
    total_expanditure <- stats["total_expanditure"][[1]]
    infoBox("Services income", round(total_expanditure, 2), icon = icon("euro-sign"), fill = FALSE, color="aqua")
  })
  output$overview_page.ui.services_mean_income <- renderInfoBox({
    stats <- service_stats()
    mean_by_client <- round(stats["mean_by_client"][[1]], 2)
    infoBox("Services mean income", mean_by_client, fill = FALSE, color = "aqua", icon = icon("euro-sign"))
  })
  output$overview_page.ui.num_services <- renderInfoBox({
    stats <- service_stats()
    num_services <- stats["num_services"][[1]]
    infoBox("N. services done", num_services, fill = FALSE, color = "aqua")
  })
  output$overview_page.ui.num_products <- renderInfoBox({
    stats <- product_stats()
    num_products <- stats["num_products"][[1]]
    infoBox("N. products sold", num_products, fill = FALSE, color = "olive")
  })
  output$overview_page.ui.products_income <- renderInfoBox({
    stats <- product_stats()
    total_expanditure <- stats["total_expanditure"][[1]]
    infoBox("Products income", round(total_expanditure, 2), fill = FALSE, color = "olive", icon = icon("euro-sign"))
  })
  output$overview_page.ui.total_income <- renderInfoBox({
    stats_p <- product_stats()
    stats_s <- service_stats()
    total_expanditure <- stats_s["total_expanditure"][[1]] + stats_p["total_expanditure"][[1]]
    infoBox("Total income", round(total_expanditure, 2), fill = FALSE, color = "red", icon = icon("euro-sign"))
  })
  
}

# HELPER FUNCTION

helper.get_condition <- function(date, freq) {
  cond_day <- date
  cond_freq <- freq
  d <- day(cond_day)
  w <- week(cond_day)
  m <- month(cond_day)
  y <- year(cond_day)
  
  if (freq == "daily") {
    conditional_filtering <- expression(day == d & month == m & year == y)
    conditional_group = "day"
  } else if (freq == "weekly") {
    conditional_filtering <- expression(week == w & month == m & year == y)
    conditional_group = "week"
  } else if (freq == "monthly") {
    conditional_filtering <- expression(month == m & year == y)
    conditional_group = "month"
  } else if (freq == "yearly") {
    conditional_filtering <- expression(year == y)
    conditional_group = "year"
  }
  return(list("cond_filter" = conditional_filtering, "cond_group" = conditional_group))
}

helper.date_env <- function(date) {
  d <- day(date)
  w <- week(date)
  m <- month(date)
  y <- year(date)
  return(list("d" = d, "w" = w, "m" = m, "y" = y))
}

helper.stats_service <- function(cond_date, freq) {
  
  condition <- helper.get_condition(cond_date, freq)
  conditional_filtering <- condition$cond_filter
  conditional_group <- condition$cond_group
  
  df.services_done %>%
    mutate(day = day(date), week = week(date), month = month(date), year = year(date)) %>%
    filter(eval(conditional_filtering, envir=helper.date_env(cond_date))) %>%
    group_by_at(conditional_group) %>%
    summarise(unique_client = n_distinct(id_costumer, day), total_expanditure = sum(price), num_services = n()) %>%
    mutate(mean_by_client = total_expanditure / unique_client)
}

helper.most.stats_service <- function(cond_date, freq) {
  
  condition <- helper.get_condition(cond_date, freq)
  conditional_filtering <- condition$cond_filter
  conditional_group <- condition$cond_group
  
  df.services_done %>%
    mutate(day = day(date), week = week(date), month = month(date), year = year(date)) %>%
    filter(eval(conditional_filtering, envir=helper.date_env(cond_date))) %>%
    group_by_at(conditional_group) %>%
    group_by(id_service) %>%
    summarise(most_used = n(), most_profitable = sum(price)) %>%
    inner_join(df.services, by = c("id_service" = "id")) %>%
    select(service_name, most_used, most_profitable)
}

helper.stats_product <- function(cond_date, freq) {
  
  condition <- helper.get_condition(cond_date, freq)
  conditional_filtering <- condition$cond_filter
  conditional_group <- condition$cond_group
  
  df.products_sold %>%
    mutate(day = day(date), week = week(date), month = month(date), year = year(date)) %>%
    filter(eval(conditional_filtering, envir=helper.date_env(cond_date))) %>%
    group_by_at(conditional_group) %>%
    summarise(total_expanditure = sum(price), num_products = n()) 
  
}

helper.most.stats_product <- function(cond_date, freq) {
  
  condition <- helper.get_condition(cond_date, freq)
  conditional_filtering <- condition$cond_filter
  conditional_group <- condition$cond_group
  
  df.products_sold %>%
    mutate(day = day(date), week = week(date), month = month(date), year = year(date)) %>%
    filter(eval(conditional_filtering, envir=helper.date_env(cond_date))) %>%
    group_by_at(conditional_group) %>%
    group_by(id_product) %>%
    summarise(most_used = n(), most_profitable = sum(price)) %>%
    inner_join(df.products, by = c("id_product" = "id")) %>%
    select(product_name, most_used, most_profitable)
}