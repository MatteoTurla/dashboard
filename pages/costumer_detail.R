library(tidyverse)
library(plotly)

costumers_page.ui <- {
  fluidPage(
    fluidRow(
      h2("Costumers"),
      DT::dataTableOutput("costumers_page.table")
    )
  )
}

costumer_detail.ui <-
  fluidPage(
    # title section
    fluidRow(
      h2(textOutput("costumer_detail.name")),
      p(textOutput("costumer_detail.info"))
    ),
    # datatable section
    fluidRow(
      DT::dataTableOutput("costumer_detail.table")
    ),
    # stats section
    h2("Statistics"),
    fluidRow(
      box(
        # box tags
        title = "Total monthly expanditure", width=12, solidHeader = TRUE, status = "primary",
        # box contents
        plotlyOutput("costumer.detail.stats.services.expanditures")
      )
    ),
    fluidRow(
      box(
        # box tags
        title = "Services", width=12, solidHeader = TRUE, status = "primary",
        # box contents
        sidebarLayout(
          sidebarPanel(
            selectInput(inputId = "costumer_detail.stats.services.name",
                        label = "Select service:",
                        choices = c("All"))
          ),
          
          # Main panel for displaying outputs ----
          mainPanel(
            plotlyOutput("costumer.detail.stats.services.plot_frequency")
          )
        )
      )
    )
  )
  
costumer_detail.server <- function(input, output, session) {
  
  # data table of all the registered costumers
  output$costumers_page.table <- DT::renderDataTable({
    df.costumers %>%
      select(first_name, last_name, email, tel)
  }, selection = "single")
  
  # information about the selected costumer
  costumer_detail_reactive <- reactive({
    row <- input$costumers_page.table_rows_selected 
    id <- df.costumers[row, "id"][[1]]
    costumer_detail <- helper.consumer_description(id)
  })
  
  # plot statistics
  output$costumer.detail.stats.services.expanditures <- renderPlotly({
    
    costumer_detail <- costumer_detail_reactive()
    services_done <- costumer_detail[[2]]
    
    print(services_done)
    
    cond_year = format(Sys.Date(), "%Y")
    cond_last_year <- as.integer(cond_year) - 1
    
    data_to_plot <- services_done %>%
      mutate(month = format(date, "%m"), year = format(date, "%Y")) %>%
      group_by(month, year) %>%
      summarise(total = sum(price))  %>%
      filter(year == cond_year | year == cond_last_year) %>%
      ungroup()
    
    plot_ly(data_to_plot, x = ~month, y = ~total, color = ~year, 
            type = 'scatter', mode = 'lines+markers')
    
  })
  
  output$costumer.detail.stats.services.plot_frequency <- renderPlotly({
    
    costumer_detail <- costumer_detail_reactive()
    services_done <- costumer_detail[[2]]
    
    cond_year = format(Sys.Date(), "%Y")
    cond_last_year <- as.integer(cond_year) - 1
    
    data_to_plot <- services_done %>%
      filter(service_name == input$costumer_detail.stats.services.name) %>%
      mutate(month = format(date, "%m"), year = format(date, "%Y"), value=1) %>%
      group_by(month, year) %>%
      summarise(total = sum(value))  %>%
      filter(year == cond_year | year == cond_last_year) %>%
      ungroup()

    plot_ly(data_to_plot, x = ~month, y = ~total, color = ~year, 
            type = 'bar') %>% 
    layout(showlegend = TRUE)
  })
  
  
  # open seleceted row and customize the selection
  observeEvent(input$costumers_page.table_rows_selected, {
    
    # row contain the index of the table
    row <- input$costumers_page.table_rows_selected 
    id <- df.costumers[row, "id"][[1]]
    
    costumer_detail <- costumer_detail_reactive()
    info <- costumer_detail[[1]]
    services_done <- costumer_detail[[2]]
    
    
    output$costumer_detail.name <- renderText({paste(info$first_name, info$last_name)})
    output$costumer_detail.info <- renderText({paste(info$tel, info$email)})
    output$costumer_detail.table <- DT::renderDataTable({
      services_done %>%
        select(date, service_name, price) %>%
        arrange(desc(date))
    })
    
    cond_year = format(Sys.Date(), "%Y")
    cond_last_year <- as.integer(cond_year) - 1
    
    services.name.select_input <- services_done %>% 
                                  mutate(year = format(date, "%Y")) %>%
                                  filter(year >= cond_last_year) %>%
                                  select(service_name) %>% 
                                  distinct(service_name) %>% 
                                  pull
    updateSelectInput(inputId = "costumer_detail.stats.services.name", 
                      choices = services.name.select_input)
    updateTabItems(session, "tabs", selected = "costumers_detail")
  }) 
  
  
}

# helper functions

# retrieve all information about the consumer
helper.consumer_description <-  function(arg_id_cliente) {
  
  costumer <- df.costumers %>%
    filter(id == arg_id_cliente)
  services_done_by_user <- df.services_done %>%
    filter(id_costumer == arg_id_cliente) %>%
    inner_join(df.services %>% select(id, service_name),
               by = c("id_service" = "id"))

  
  result <- list(costumer, services_done_by_user)
  return(result)
}


