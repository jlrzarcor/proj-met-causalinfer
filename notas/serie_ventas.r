# CODE FOR PROJECT

#library(readr)
#library(lubridate)
#library(tidyverse)
#library(fabletools)
#library(zoo)
#library(ggplot2)
#library(tsibble)
#library(feasts)
#library(bsts)


sales <- read_delim("../datos/sales.csv")


# ======================= CREATE DATABASE

sales_m <- sales |>
    mutate(t = (year(date)-year(min(date)))*12 + (month(date)-month(min(date)))) |>
    mutate(date = yearmonth(date)) |>
    group_by(date,t,country,store,product) |>
    summarise(num_sold = sum(num_sold,na.rm = TRUE)) |>
    ungroup() |>
    as_tsibble(index = date,key = c(country,store,product))

# ======================= BASIC GRAPHS

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



# ======================= START FORECASTING



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
      ajuste <- bsts(y, state.specification = esp_estado,niter = 1000, seed = 230430)
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
      ajuste <- bsts(y, state.specification = esp_estado,niter = 1000, seed = 230430)
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
      ajuste <- bsts(y, state.specification = esp_estado,niter = 1000, seed = 230430)
      TS[[paste(c,sep = "_")]] <- ajuste
}

y <-sales_m |> 
  aggregate_key(country / store /product, num_sold = sum(num_sold,na.rm=TRUE)) |>
  filter(is_aggregated(product),is_aggregated(store),is_aggregated(country)) |>
  as.ts()
esp_estado <- 
  AddLocalLinearTrend(list(), y) |> 
  AddSeasonal(y, nseasons = 12) 
ajuste <- bsts(y, state.specification = esp_estado,niter = 1000, seed = 230430)
TS[["total"]] <- ajuste

saveRDS(TS, file = "../datos/TS.RDS") 
TS<-readRDS(file = "../datos/TS.RDS") 



ajuste <- TS[["total"]]
dims <- ajuste$state.contributions |> dim()
tiempo <- dims[3]
contribuciones_tbl <- map(1:tiempo, ~ ajuste$state.contributions[,,.x] |> 
  as_tibble() |> mutate(t = .x)) |> bind_rows() |> 
  pivot_longer(trend:seasonal.12.1, values_to = "value", names_to = "comp") |> 
  group_by(t, comp) |> 
  summarise(media = mean(value), q5 = quantile(value, 0.05),
            q95 = quantile(value, 0.95), .groups = "drop")

#u<-serie1$state.specification
#u[1,1]

#head(contribuciones_tbl)
#plot(serie1, "state", burn = 500)


#u<-serie1$
#u[1,1]




y <- sales_m |> filter(country=="Poland",product=="Kaggle Advanced Techniques",store=="KaggleMart") |> select(num_sold) |> as.ts()
esp_estado <- 
  AddLocalLinearTrend(list(), y) |> 
  AddSeasonal(y, nseasons = 12) 

ajuste <- bsts(y, state.specification = esp_estado,
               niter = 10000, seed = 230430)

dims <- ajuste$state.contributions |> dim()
tiempo <- dims[3]
contribuciones_tbl <- map(1:tiempo, ~ ajuste$state.contributions[,,.x] |> 
  as_tibble() |> mutate(t = .x)) |> bind_rows() |> 
  pivot_longer(trend:seasonal.12.1, values_to = "value", names_to = "comp") |> 
  group_by(t, comp) |> 
  summarise(media = mean(value), q5 = quantile(value, 0.05),
            q95 = quantile(value, 0.95), .groups = "drop")

ggplot(contribuciones_tbl, 
  aes(x = t, y = media, ymin = q5, ymax = q95)) +
  geom_ribbon(alpha = 0.1) + 
  geom_line(alpha = 1) + facet_wrap(~ comp, scales = "free_y", ncol = 1)













mod_spec_1 <- AddLocalLevel(list(), y )

mod_spec_2 <- AddLocalLinearTrend(list(), y)

mod_spec_3 <- AddLocalLevel(list(), y) |> 
  AddSeasonal(nseasons = 12, y)

mod_spec_4 <- AddLocalLinearTrend(list(), y) |> 
  AddSeasonal(nseasons = 12, y) 

mod_spec_5 <- AddSemilocalLinearTrend(list(), y) |> 
  AddSeasonal(nseasons = 12, y) 

mod_spec_6 <- AddSemilocalLinearTrend(list(), y) |> 
  AddSeasonal(nseasons = 12, y) |> 
  AddAr(lags = 2, y)

specs <- list(mod_spec_1, mod_spec_2, mod_spec_3, mod_spec_4, mod_spec_5, mod_spec_6)

ajustes <- map(specs, function(spec){
  bsts(y, spec, niter = 1000, ping = 10000)
})



CompareBstsModels(ajustes, burn = 500)


grViz("
digraph {
  graph [ranksep = 0.7, fontsize = 11]
  node [shape = circle, style = filled, width = 1, fontcolor = blue, fontname = Helvetica, fontsize = 19]
  node [color = green]
    y  [label = Total]
  node [color = steelblue]
    A  [label = A]
    AA [label = AA]
    AB [label = AB]
    AC [label = AC]
  node [color = yellow]
    B  [label = B]
    BA [label = BA]
    BB [label = BC]
  edge [minlen = 2, color = gray, arrowsize = 0]
    y -> A
    y -> B
    A -> AA
    A -> AB
    A -> AC
    B -> BA
    B -> BB
    
{ rank = same; A; B }
{ rank = same; AA; AB; AC; BA; BB }
}
", width = 600, height = 300
)




grViz("
digraph {
  graph [ranksep = 0.7, fontsize = 11]
  node [shape = circle, style = filled, width = 1, fontcolor = blue, fontname = Helvetica, fontsize = 19]
  node [color = green]
    T1  [label = Total]
    T2  [label = Total]
  node [color = steelblue]
    A  [label = A]
    AX [label = AX]
    AY [label = AY]
  node [color = yellow]
    B  [label = B]
    BX [label = BX]
    BY [label = BY]
  node [color = steelblue]
    X  [label = X]
    AX2 [label = AX]
    BX2 [label = BX]
  node [color = yellow]
    Y  [label = Y]
    AY2 [label = AY]
    BY2 [label = BY]
  edge [minlen = 2, color = gray, arrowsize = 0]
    T1 -> A
    T1 -> B
    T2 -> X
    T2 -> Y
    A -> AX
    A -> AY
    B -> BX
    B -> BY
    X -> AX2
    X -> BX2
    Y -> AY2
    Y -> BY2

    
{ rank = same; A; B }
{ rank = same; AX; AY; BX; BY; AX2; AY2; BX2; BY2}
}
", width = 600, height = 300
)

