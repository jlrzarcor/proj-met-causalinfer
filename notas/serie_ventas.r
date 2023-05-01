library(readr)
library(dplyr)
library(lubridate)
library(fabletools)
library(zoo)
library(ggplot2)
library(tsibble)
library(feasts)
library(bsts)

sales <- read_delim("../datos/sales.csv")
sales_m <- sales |>
    mutate(t = (year(date)-year(min(date)))*12 + (month(date)-month(min(date)))) |>
    mutate(date = yearmonth(date)) |>
    group_by(date,t,country,store,product) |>
    summarise(num_sold = sum(num_sold,na.rm = TRUE)) |>
    ungroup() |>
    as_tsibble(index = date,key = c(country,store,product))


sales_m |>
  aggregate_key(country / store /product, num_sold = sum(num_sold,na.rm=TRUE)/1000) |>
  filter(is_aggregated(store)) |>
  autoplot(num_sold) +
  labs(x = "Mes/Año" , y = "Ventas (miles)",title = "Ventas totales y por país") +
  facet_wrap(vars(country), scales = "free_y", ncol = 3) +
  theme(legend.position = "none")



sales_m |>
  aggregate_key(country / store /product, num_sold = sum(num_sold,na.rm=TRUE)/1000) |>
  filter(is_aggregated(store)) |>
  autoplot(num_sold) +
  labs(x = "Mes/Año" , y = "Ventas (miles)",title = "Ventas totales y por país") +
  facet_wrap(vars(country), scales = "free_y", ncol = 3) +
  theme(legend.position = "none")
  

sales_m |>
  aggregate_key(country / store /product, num_sold = sum(num_sold,na.rm=TRUE)/1000) |>
  filter(country == "Poland" | country == "Germany") |>
  #filter(is_aggregated(store)) |>
  autoplot(num_sold) +
  labs(x = "Mes/Año" , y = "Ventas (miles)",title = "Ventas de Polonia por tienda/producto") +
  facet_wrap(vars(country,store), scales = "free_y", ncol = 3) +
  theme(legend.position = "none")



sales_m |>
  aggregate_key(country / store /product, num_sold = sum(num_sold,na.rm=TRUE)/1000) |>
  filter(is_aggregated(product),is_aggregated(store)) |>
  select(-product,-store) |>
  mutate(country = factor(country)) |>
  gg_season(num_sold) +
  facet_wrap(vars(country), nrow = 2, scales = "free_y")+
  labs(x = "Mes",y = "Ventas (miles)")



esp_estado <- 
  AddLocalLinearTrend(list(), y) |> 
  AddSeasonal(y, nseasons = 12) 

ajuste <- bsts(y, state.specification = esp_estado,
               niter = 10000, seed = 230430)

