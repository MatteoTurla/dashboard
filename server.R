if (!require("pacman")) install.packages("pacman")
pacman::p_load(shiny, shinydashboard)

source("pages/costumer_detail.R", local=TRUE)
source("pages/overview.R", local=TRUE)
source("pages/products.R", local=TRUE)
source("pages/services.R", local=TRUE)

server <- function(input, output, session) {
  
  overview_page.server(input, output, session)
  costumer_detail.server(input, output, session)
  products_page.server(input, output, session)
  services_page.server(input, output, session)
  
}