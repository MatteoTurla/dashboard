library(tidyverse)
library(plotly)
library(stringr)

services_page.ui <- {
  fluidPage(
    fluidRow(
      h2("Services"),
      DT::dataTableOutput("services_page.ui.table")
    ),
    fluidRow(
      column(6, h2("Statistics")),
      column(3, textInput("services_page.ui.select.filter", 
                          label = "Filter selection:")),
      column(3, selectInput("services_page.ui.select.selection", 
                            label = "Select service",
                            choices = c("All")))
    ),
    fluidRow(
      box(
        # box tags
        title = "Income", width=12, solidHeader = TRUE, status = "primary",
        # box contents
        plotlyOutput("services_page.ui.income.plot")
      )
    ),
    fluidRow(
      box(
        # box tags
        title = "Count", width=12, solidHeader = TRUE, status = "primary",
        # box contents
        plotlyOutput("services_page.ui.count.plot")
      )
    )
  )
}

services_page.server <- function(input, output, session) {
  
  output$services_page.ui.table <- DT::renderDataTable({
    df.services %>%
      select(service_name)
  })
  
  # add All to filtering product
  df.services_name <- df.services %>%
    add_row(id = -1, service_name = "All") %>%
    arrange(id) %>%
    select(service_name)
  
  # update selection
  updateSelectInput(inputId = "services_page.ui.select.selection", 
                    choices = df.services_name %>%
                      select(service_name) %>%
                      pull)
  
  # filter selection
  observeEvent(input$services_page.ui.select.filter, {
    
    updateSelectInput(inputId = "services_page.ui.select.selection", 
                      choices = df.services_name %>%
                        select(service_name) %>%
                        filter(str_detect(tolower(service_name), 
                                          tolower(input$services_page.ui.select.filter))) %>%
                        pull)
  })
  
  get_selected_service <-reactive({
    selected <- input$services_page.ui.select.selection
    if (selected == "All") {
      df.services_done
    } else {
      service_id <- df.services %>%
        filter(service_name == selected) %>%
        select(id)
      df.services_done %>% 
        filter(id_service == service_id[[1]] )
    }
  })
  
  # PLOT
  output$services_page.ui.income.plot <- renderPlotly({
    
    cond_year = format(Sys.Date(), "%Y")
    cond_last_year <- as.integer(cond_year) - 1
    
    selected <- get_selected_service() 
    
    data_to_plot <- selected %>%
      mutate(month = format(date, "%m"), year = format(date, "%Y")) %>%
      group_by(month, year) %>%
      summarise(total = sum(price))  %>%
      filter(year == cond_year | year == cond_last_year) %>%
      ungroup()
    
    plot_ly(data_to_plot, x = ~month, y = ~total, color = ~year, 
            type = 'scatter', mode = 'lines+markers') %>%
      layout(title = input$services_page.ui.select.selection, 
             showlegend =  TRUE,
             yaxis = list(title="Income"))
  })
  
  output$services_page.ui.count.plot <- renderPlotly({
    
    cond_year = format(Sys.Date(), "%Y")
    cond_last_year <- as.integer(cond_year) - 1
    
    selected <- get_selected_service() 
    
    data_to_plot <- selected %>%
      mutate(month = format(date, "%m"), year = format(date, "%Y"), value=1) %>%
      group_by(month, year) %>%
      summarise(total = sum(value))  %>%
      filter(year == cond_year | year == cond_last_year) %>%
      ungroup()
    
    plot_ly(data_to_plot, x = ~month, y = ~total, color = ~year, 
            type = 'bar') %>% 
      layout(title = input$services_page.ui.select.selection, 
             showlegend =  TRUE,
             yaxis = list(title="Frequency"))
    
  })
  
  
}