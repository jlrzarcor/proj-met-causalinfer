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

# ======================= CREATE TS

if(sum(list.files("../datos/")=="TS.RDS")==0){
  
  TS<-list()

  for(c in unique(sales_m$country)){
    for(s in unique(sales$store)){
      for(p in unique(sales$product)){
        y <-sales_m |> 
          filter(country==c,product==p,store==s) |> 
          select(num_sold) |> 
          as.ts()
        esp_estado <- 
          AddLocalLinearTrend(list(), y) |> 
          AddSeasonal(y, nseasons = 12) 
        ajuste <- bsts(y, state.specification = esp_estado,niter = 10000, seed = 230430)
        TS[[paste(c,s,p,sep = "_")]] <- ajuste
      }
    }
  }

  for(c in unique(sales_m$country)){
    for(s in unique(sales$store)){
      y <-sales_m |>
        group_by(t,country,store) |>
        summarise(num_sold = sum(num_sold,na.rm = TRUE)) |> 
        ungroup() |>
        filter(country==c,store==s) |> 
        as_tsibble(index = date,key = c(country,store)) |>
        select(num_sold) |> 
        as.ts()
      esp_estado <- 
        AddLocalLinearTrend(list(), y) |> 
        AddSeasonal(y, nseasons = 12) 
      ajuste <- bsts(y, state.specification = esp_estado,niter = 10000, seed = 230430)
      TS[[paste(c,s,sep = "_")]] <- ajuste
    }
  }

  for(c in unique(sales_m$country)){
    y <-sales_m |> 
      group_by(t,country) |>
      summarise(num_sold = sum(num_sold,na.rm = TRUE)) |>  
      ungroup() |>
      filter(country==c) |> 
      as_tsibble(index = date,key = c(country)) |>
      select(num_sold) |> 
      as.ts()
    esp_estado <- 
      AddLocalLinearTrend(list(), y) |> 
      AddSeasonal(y, nseasons = 12) 
    ajuste <- bsts(y, state.specification = esp_estado,niter = 10000, seed = 230430)
    TS[[paste(c,sep = "_")]] <- ajuste
  }
  
  y <-sales_m |> 
    aggregate_key(country / store /product, num_sold = sum(num_sold,na.rm=TRUE)) |>
    filter(is_aggregated(product),is_aggregated(store),is_aggregated(country)) |>
    as.ts()
  esp_estado <- 
    AddLocalLinearTrend(list(), y) |> 
    AddSeasonal(y, nseasons = 12) 
  ajuste <- bsts(y, state.specification = esp_estado,niter = 10000, seed = 230430)
  TS[["total"]] <- ajuste

  saveRDS(TS, file = "../datos/TS.RDS") 
}



if(sum(list.files("../datos/")=="TSp.RDS")==0){
  
  TSp<-list()

  for(c in unique(sales_m$country)){
    for(s in unique(sales$store)){
      y1 <-sales_m |> 
        filter(country==c,store==s) |> 
        select(product,num_sold) |> 
        index_by(date) |>
        mutate(total = sum(num_sold)) |>
        mutate(pp = num_sold/total) 
      for(p in unique(sales$product)){
        y <-y1 |> 
          filter(product==p) |> 
          select(pp) |> 
          as.ts()
        esp_estado <- 
          AddLocalLinearTrend(list(), y) |> 
          AddSeasonal(y, nseasons = 12) 
        ajuste <- bsts(y, state.specification = esp_estado,niter = 10000, seed = 230430)
        TSp[[paste(c,s,p,sep = "_")]] <- ajuste
      }
    }
  }
  
  saveRDS(TSp, file = "../datos/TSp.RDS") 
  
}
