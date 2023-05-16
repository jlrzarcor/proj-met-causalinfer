# CODE FOR PROJECT

## ID_F0 --> This function aim to install and load libraries required.
rm(list = ls())

instalar <- function(paquete) {
  if (!require(paquete,character.only = TRUE, quietly = TRUE,
               warn.conflicts = FALSE)) {
    install.packages(as.character(paquete), dependecies = TRUE,
                     repos = "http://cran.us.r-project.org")
    library(paquete, character.only = TRUE, quietly = TRUE,
            warn.conflicts = FALSE)
  }
}

## *******   Defining required libraries to install and load   *******

## Use this vector to indicate libraries to load.
paquetes <- c("tidyverse", "lubridate", "glmnet", "fpp3", "bsts", "DiagrammeR", "hts","knitr")
invisible(lapply(paquetes, instalar))

sales <- read_delim("../datos/sales.csv")

# ======================= CREATE DATABASE

sales_m <- sales |>
    mutate(t = (year(date)-year(min(date)))*12 + (month(date)-month(min(date)))) |>
    mutate(date = yearmonth(date)) |>
    group_by(date,t,country,store,product) |>
    summarise(num_sold = sum(num_sold,na.rm = TRUE)) |>
    ungroup() |>
    as_tsibble(index = date,key = c(country,store,product))
