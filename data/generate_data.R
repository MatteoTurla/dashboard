library(tidyverse, warn.conflicts = FALSE)
library(plotly, warn.conflicts = FALSE)
library(lubridate, warn.conflicts = FALSE)
library(parallel, warn.conflicts = FALSE)


services <- read_csv("mock/services.csv")
products <- read_csv("mock/products.csv")
costumers <- read_csv("mock/costumers.csv")

dates <- c(seq(as.Date('2020-01-01'),as.Date('2021-05-31'),by = 1))


# not parallel computation
generate_interactions <- function(dates) {
  
  services_done <- tibble(id_service = integer(), id_costumer = integer(), price = numeric(), date = as.Date(integer(), origin="2020-01-01"))
  products_sold <- tibble(id_product = integer(), id_costumer = integer(), price = numeric(), date = as.Date(integer(), origin="2020-01-01"))
  
  for (i in 1:length(dates)) {
    date <- dates[i]
    n <- as.integer(rnorm(1, mean = 15, sd = 15/3))
    if (n < 0) n <- 0
    sample_costumers <- costumers %>%
      sample_n(n) %>%
      pull(id)
    
    for (sample_costumer in sample_costumers) {
      k <- as.integer(rnorm(1, mean = 3, sd = 1))
      if (k < 1) {
        k <- 1
      }
      sample_services <- services %>%
        sample_n(k) %>%
        mutate(id_service = id, 
               id_costumer = as.integer(sample_costumer),
               price = round(runif(1, -3, 3), 2) + price,
               date = date
        ) %>%
        select(id_service, id_costumer, price, date)
      
      services_done <- services_done %>% bind_rows(sample_services)
      
      k <- as.integer(rnorm(1, mean = 0, sd = 1))
      if (k < 0) {
        k <- 0
      }
      
      sample_products <- products %>%
        sample_n(k) %>%
        mutate(id_product = id, 
               id_costumer = as.integer(sample_costumer),
               price = price,
               date = date
        ) %>%
        select(id_product, id_costumer, price, date)
      
      products_sold <- products_sold %>% bind_rows(sample_products)
      
    }
  }
  return(list("services_done" = services_done, "products_sold" = products_sold))
}

# not parallel computation
t0 <- proc.time()
interactions <- generate_interactions(dates)
services_done <- interactions$services_done
products_sold <- interactions$products_sold
t1 <- proc.time()
t1 - t0

# parallel functions
generate_interactions_service_parallel <- function(date) {
  
  sample_service_interactions <- function(id_costumer) {
    k <- as.integer(rnorm(1, mean = 3, sd = 1))
    if (k < 1) {
      k <- 1
    }
    sample_services <- services %>%
      sample_n(k) %>%
      mutate(id_service = id, 
             id_costumer = as.integer(id_costumer),
             price = round(runif(1, -3, 3), 2) + price,
             date = date
      ) %>%
      select(id_service, id_costumer, price, date)
  }
  
  n <- as.integer(rnorm(1, mean = 15, sd = 15/3))
  if (n < 0) n <- 0
  sample_costumers <- costumers %>%
    sample_n(n) %>%
    pull(id)
  
  lapply(sample_costumers, sample_service_interactions)   
}

generate_interactions_product_parallel <- function(date) {
  
  sample_product_interactions <- function(id_costumer) {
    k <- as.integer(rnorm(1, mean = 0, sd = 1))
    if (k < 0) {
      k <- 0
    }
    
    sample_products <- products %>%
      sample_n(k) %>%
      mutate(id_product = id, 
             id_costumer = as.integer(id_costumer),
             price = price,
             date = date
      ) %>%
      select(id_product, id_costumer, price, date)
  }
  
  n <- as.integer(rnorm(1, mean = 15, sd = 15/3))
  if (n < 0) n <- 0
  sample_costumers <- costumers %>%
    sample_n(n) %>%
    pull(id)
  
  lapply(sample_costumers, sample_product_interactions)   
}

#parallel comutation
t0 <- proc.time()

cores <- (detectCores() - 2) 

products_sold <- mclapply(dates, FUN = generate_interactions_product_parallel, mc.cores = cores) %>% bind_rows() 
services_done <- mclapply(dates, FUN = generate_interactions_service_parallel, mc.cores = cores) %>% bind_rows() 

t1 <- proc.time()

t1 - t0

write.csv(services_done, "mock/services_done.csv", row.names=FALSE)
write.csv(products_sold, "mock/products_sold.csv", row.names=FALSE)
