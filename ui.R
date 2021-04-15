## ui.R ##
if (!require("pacman")) install.packages("pacman")
pacman::p_load(shiny, shinydashboard)

source("pages/costumer_detail.R", local=TRUE)
source("pages/overview.R", local=TRUE)
source("pages/products.R", local=TRUE)
source("pages/services.R", local=TRUE)

sidebar <- dashboardSidebar(
  sidebarMenu(
    id = "tabs",
    menuItem("Overview", tabName = "overview"),
    menuItem("Costumers", tabName = "costumers"),
    menuItem("Services",  tabName = "services"),
    menuItem("Products",  tabName = "products"),
    
    tags$div(style="display:none;",
             menuItem("Costumer Page", tabName = "costumers_detail")
    )
  )
)

body <- dashboardBody(
  tabItems(
    tabItem(tabName = "overview", overview_page.ui),
    tabItem(tabName = "costumers", costumers_page.ui),
    tabItem(tabName = "services", services_page.ui),
    tabItem(tabName = "products", products_page.ui),
    tabItem(tabName = "costumers_detail", costumer_detail.ui)
  )
)

# Put them together into a dashboardPage
ui <- dashboardPage(
  dashboardHeader(title = "Dashboard"),
  sidebar,
  body
)
