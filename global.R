if (!require("pacman")) install.packages("pacman")
pacman::p_load(shiny, shinydashboard, tidyverse)

df.costumers <- read_csv("data/mock/costumers.csv")
df.services <- read_csv("data/mock/services.csv")
df.products <- read_csv("data/mock/products.csv")
df.services_done <- read_csv("data/mock/services_done.csv")
df.products_sold <- read_csv("data/mock/products_sold.csv")


