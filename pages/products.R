library(tidyverse)
library(plotly)
library(stringr)

products_page.ui <- {
  fluidPage(
    fluidRow(
      h2("Products"),
      DT::dataTableOutput("products_page.ui.table")
    ),
    fluidRow(
      column(6, h2("Statistics")),
      column(3, textInput("products_page.ui.select.filter", 
                            label = "Filter selection:")),
      column(3, selectInput("products_page.ui.select.selection", 
                            label = "Select product",
                            choices = c("All")))
    ),
    fluidRow(
      box(
        # box tags
        title = "Income", width=12, solidHeader = TRUE, status = "primary",
        # box contents
        plotlyOutput("products_page.ui.income.plot")
      )
    ),
    fluidRow(
      box(
        # box tags
        title = "Count", width=12, solidHeader = TRUE, status = "primary",
        # box contents
        plotlyOutput("products_page.ui.count.plot")
      )
    )
  )
}

products_page.server <- function(input, output, session) {
  
  output$products_page.ui.table <- DT::renderDataTable({
    df.products %>%
      select(product_name)
  })
  
  # add All to filtering product
  df.product_name <- df.products %>%
    add_row(id = -1, product_name = "All") %>%
    arrange(id) %>%
    select(product_name)
    
  
  # update selection
  updateSelectInput(inputId = "products_page.ui.select.selection", 
                    choices = df.product_name %>%
                      select(product_name) %>%
                      pull)
  
  # filter selection
  observeEvent(input$products_page.ui.select.filter, {
    
    updateSelectInput(inputId = "products_page.ui.select.selection", 
                      choices = df.product_name %>%
                        select(product_name) %>%
                        filter(str_detect(tolower(product_name), 
                                          tolower(input$products_page.ui.select.filter))) %>%
                        pull)
  })
  
  get_selected_product <-reactive({
    selected <- input$products_page.ui.select.selection
    if (selected == "All") {
      df.products_sold
    } else {
      product_id <- df.products %>%
        filter(product_name == selected) %>%
        select(id)
      df.products_sold %>% 
        filter(id_product == product_id[[1]] )
    }
  })
  
  # PLOT
  output$products_page.ui.income.plot <- renderPlotly({
    
    cond_year = format(Sys.Date(), "%Y")
    cond_last_year <- as.integer(cond_year) - 1
    
    selected <- get_selected_product() 
    
    data_to_plot <- selected %>%
      mutate(month = format(date, "%m"), year = format(date, "%Y")) %>%
      group_by(month, year) %>%
      summarise(total = sum(price))  %>%
      filter(year == cond_year | year == cond_last_year) %>%
      ungroup()
    
    plot_ly(data_to_plot, x = ~month, y = ~total, color = ~year, 
            type = 'scatter', mode = 'lines+markers') %>%
      layout(title = input$products_page.ui.select.selection, 
             showlegend =  TRUE,
             yaxis = list(title="Income"))
  })
  
  output$products_page.ui.count.plot <- renderPlotly({
    
    cond_year = format(Sys.Date(), "%Y")
    cond_last_year <- as.integer(cond_year) - 1
    
    selected <- get_selected_product() 
    
    data_to_plot <- selected %>%
      mutate(month = format(date, "%m"), year = format(date, "%Y"), value=1) %>%
      group_by(month, year) %>%
      summarise(total = sum(value))  %>%
      filter(year == cond_year | year == cond_last_year) %>%
      ungroup()
    
    plot_ly(data_to_plot, x = ~month, y = ~total, color = ~year, 
            type = 'bar') %>% 
      layout(title = input$products_page.ui.select.selection, 
             showlegend =  TRUE,
             yaxis = list(title="Frequency"))
    
  })
  

}