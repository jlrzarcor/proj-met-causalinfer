# CODE FOR PROJECT

library(readr)
library(lubridate)
library(tidyverse)
library(fabletools)
#library(zoo)
#library(ggplot2)
library(tsibble)
library(feasts)
library(bsts)


sales <- read_delim("../datos/sales.csv")
#sales<-read_delim("/home/urielmtzsa/itam/semestre4/metodos_analiticos/proyecto/datos/sales.csv") 

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
  labs(x = "Mes/Año" , y = "Ventas (miles)",title = "Ventas de Polonia/Alemania por tienda/producto") +
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
TS<-readRDS(file = "../datos/TS.RDS") 
#saveRDS(TS, file = "/home/urielmtzsa/itam/semestre4/metodos_analiticos/proyecto/datos/TS.RDS") 
#TS<-readRDS(file = "/home/urielmtzsa/itam/semestre4/metodos_analiticos/proyecto/datos/TS.RDS") 

# ======================= ANÁLISIS serie TOTAL

ajuste <- TS[["total"]]
plot(ajuste, "state", burn = 2000)

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

pred_errors_tbl <- 
  ajuste$one.step.prediction.errors |> 
  t() |> as_tibble() |>
  mutate(t = 1: tiempo) |> 
  pivot_longer(-c(t), names_to = "sim", values_to = "valor") |> 
  group_by(t) |> 
  summarise(valor = mean(valor)) |> 
  as_tsibble(index = t)

ACF(pred_errors_tbl) |> 
  autoplot() + ylim(c(-1,1))


pred <- predict(ajuste, horizon = 24,burn=2000)
plot(pred)

error <- ajuste$one.step.prediction.errors |> apply(2, mean)
qqnorm(error)
qqline(error)


# ======================= Total vs Producto vs Tienda vs País

ajuste <- TS[["total"]]
dims <- ajuste$state.contributions |> dim()
tiempo <- dims[3]
contribuciones_total <- map(1:tiempo, ~ ajuste$state.contributions[,,.x] |> 
  as_tibble() |> mutate(t = .x)) |> bind_rows() |> 
  pivot_longer(trend:seasonal.12.1, values_to = "value", names_to = "comp") |> 
  group_by(t, comp) |> 
  summarise(media = mean(value), q5 = quantile(value, 0.05),
            q95 = quantile(value, 0.95), .groups = "drop") |>
  group_by(t) |>
  summarise(media_total = sum(media))
pred <- predict(ajuste, horizon = 24,burn=2000)$mean
contribuciones_total<-rbind(contribuciones_total,tibble(t = 49:72, media_total = pred))

i<-0
for(c in unique(sales_m$country)){
  for(s in unique(sales$store)){
    for(p in unique(sales$product)){
      ajuste <- TS[[paste(c,s,p,sep = "_")]]
      dims <- ajuste$state.contributions |> dim()
      tiempo <- dims[3]
      contribuciones_tbl <- map(1:tiempo, ~ ajuste$state.contributions[,,.x] |> 
        as_tibble() |> mutate(t = .x)) |> bind_rows() |> 
        pivot_longer(trend:seasonal.12.1, values_to = "value", names_to = "comp") |> 
        group_by(t, comp) |> 
        summarise(media = mean(value), q5 = quantile(value, 0.05),
                  q95 = quantile(value, 0.95), .groups = "drop") |>
        group_by(t) |>
        summarise(media_producto = sum(media)) |>
        mutate(p = paste(c,s,p,sep = "_"))
        pred <- predict(ajuste, horizon = 24,burn=2000)$mean
        contribuciones_tbl<-rbind(contribuciones_tbl,tibble(t = 49:72, media_producto = pred,p = paste(c,s,p,sep = "_")))
        if(i==0){contribuciones_producto<-contribuciones_tbl}else{contribuciones_producto<-rbind(contribuciones_producto,contribuciones_tbl)}
        i<-i+1
    }
  }
}
contribuciones_producto_tbl<-contribuciones_producto
contribuciones_producto<-contribuciones_producto |>
  group_by(t) |>
  summarise(media_producto = sum(media_producto)) 

i<-0
for(c in unique(sales_m$country)){
  for(s in unique(sales$store)){
    ajuste <- TS[[paste(c,s,sep = "_")]]
    dims <- ajuste$state.contributions |> dim()
    tiempo <- dims[3]
    contribuciones_tbl <- map(1:tiempo, ~ ajuste$state.contributions[,,.x] |> 
      as_tibble() |> mutate(t = .x)) |> bind_rows() |> 
      pivot_longer(trend:seasonal.12.1, values_to = "value", names_to = "comp") |> 
      group_by(t, comp) |> 
      summarise(media = mean(value), q5 = quantile(value, 0.05),
                q95 = quantile(value, 0.95), .groups = "drop") |>
      group_by(t) |>
      summarise(media_store = sum(media)) |>
      mutate(p = paste(c,s,sep = "_"))
      pred <- predict(ajuste, horizon = 24,burn=2000)$mean
      contribuciones_tbl<-rbind(contribuciones_tbl,tibble(t = 49:72, media_store = pred,p = paste(c,s,sep = "_")))
      if(i==0){contribuciones_store<-contribuciones_tbl}else{contribuciones_store<-rbind(contribuciones_store,contribuciones_tbl)}
      i<-i+1
  }
}
contribuciones_store_tbl<-contribuciones_store
contribuciones_store<-contribuciones_store |>
  group_by(t) |>
  summarise(media_store = sum(media_store)) 


i<-0
for(c in unique(sales_m$country)){
  ajuste <- TS[[paste(c,sep = "_")]]
  dims <- ajuste$state.contributions |> dim()
  tiempo <- dims[3]
  contribuciones_tbl <- map(1:tiempo, ~ ajuste$state.contributions[,,.x] |> 
    as_tibble() |> mutate(t = .x)) |> bind_rows() |> 
    pivot_longer(trend:seasonal.12.1, values_to = "value", names_to = "comp") |> 
    group_by(t, comp) |> 
    summarise(media = mean(value), q5 = quantile(value, 0.05),
              q95 = quantile(value, 0.95), .groups = "drop") |>
    group_by(t) |>
    summarise(media_country = sum(media)) |>
    mutate(p = paste(c,sep = "_"))
    pred <- predict(ajuste, horizon = 24,burn=2000)$mean
    contribuciones_tbl<-rbind(contribuciones_tbl,tibble(t = 49:72, media_country = pred,p = paste(c,sep = "_")))
    if(i==0){contribuciones_country<-contribuciones_tbl}else{contribuciones_country<-rbind(contribuciones_country,contribuciones_tbl)}
    i<-i+1
}
contribuciones_country_tbl <- contribuciones_country
contribuciones_country<-contribuciones_country |>
  group_by(t) |>
  summarise(media_country = sum(media_country)) 

contribuciones_tbl<-cbind(
  contribuciones_total,
  contribuciones_producto|>select(media_producto),
  contribuciones_store |>select(media_store),
  contribuciones_country |>select(media_country)
)
contribuciones_tbl<-contribuciones_tbl |>
  mutate(
    diff_total_producto = media_total - media_producto,
    diff_total_store = media_total - media_store,
    diff_total_country = media_total - media_country
  )

contribuciones_tbl_2<-contribuciones_producto_tbl |>
  separate(p, c("country","store","product"),sep="_") |>
  group_by(t,country) |>
  summarise(media_producto = sum(media_producto)) |>
  ungroup()
contribuciones_tbl_2 <- merge(contribuciones_tbl_2,contribuciones_country_tbl, by.x = c("t","country"),by.y =c("t", "p"),all=TRUE) 


contribuciones_tbl_3<-contribuciones_store_tbl |>
  separate(p, c("country","store"),sep="_") |>
  group_by(t,country) |>
  summarise(media_store = sum(media_store)) |>
  ungroup()
contribuciones_tbl_3 <- merge(contribuciones_tbl_3,contribuciones_country_tbl, by.x = c("t","country"),by.y =c("t", "p"),all=TRUE) 


ggplot(contribuciones_tbl)+ 
  geom_line(aes(x = t, y = media_total/1000,colour = "Total") ) +
  geom_line(aes(x = t, y = media_producto/1000, colour = "Producto"),linetype = "dashed",size = 1) +
  labs(x = "t", y = "sales (miles)") +
  scale_color_manual(name = "Serie Jerárquica", values = c("Total" = "red", "Producto" = "darkblue")) + 
  geom_vline(xintercept = 48, linetype="dotted", color = "black", size=1.5)

ggplot(contribuciones_tbl)+ 
  geom_line(aes(x = t, y = diff_total_producto/1000,colour = "Total - Producto")) +
  geom_line(aes(x = t, y = diff_total_store/1000,colour = "Total - Store")) +
  geom_line(aes(x = t, y = diff_total_country/1000,colour = "Total - Country")) +
  labs(x = "t", y = "sales (miles)") +
  scale_color_manual(name = "Diferencias", values = c(
    "Total - Producto" = "darkblue","Total - Store" = "#218611","Total - Country" ="#77091b")) +
  geom_hline(yintercept = 0, linetype="dotted", color = "black", size=1.5) +
  geom_vline(xintercept = 48, linetype="dotted", color = "black", size=1.5)


ggplot(contribuciones_tbl_2)+ 
  geom_line(aes(x = t, y = media_country/1000,colour = "Country") ) +
  geom_line(aes(x = t, y = media_producto/1000, colour = "Producto"),linetype = "dashed",size = 1) +
  labs(x = "t", y = "sales (miles)") +
  scale_color_manual(name = "Serie Jerárquica", values = c("Country" = "red", "Producto" = "darkblue")) + 
  geom_vline(xintercept = 48, linetype="dotted", color = "black", size=1.5) +
  facet_wrap(vars(country), nrow = 3, scales = "free_y")





ggplot(contribuciones_tbl)+ 
  geom_line(aes(x = t, y = media_total/1000,colour = "Total") ) +
  geom_line(aes(x = t, y = media_store/1000, colour = "Store"),linetype = "dashed",size = 1) +
  labs(x = "t", y = "sales (miles)") +
  scale_color_manual(name = "Serie Jerárquica", values = c("Total" = "red", "Store" = "darkblue")) + 
  geom_vline(xintercept = 48, linetype="dotted", color = "black", size=1.5)

ggplot(contribuciones_tbl_3)+ 
  geom_line(aes(x = t, y = media_country/1000,colour = "Country") ) +
  geom_line(aes(x = t, y = media_store/1000, colour = "Store"),linetype = "dashed",size = 1) +
  labs(x = "t", y = "sales (miles)") +
  scale_color_manual(name = "Serie Jerárquica", values = c("Country" = "red", "Store" = "darkblue")) + 
  geom_vline(xintercept = 48, linetype="dotted", color = "black", size=1.5) +
  facet_wrap(vars(country), nrow = 3, scales = "free_y")


# ======================= Proporciones de tiendas sobre productos

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

saveRDS(TSp, file = "/home/urielmtzsa/itam/semestre4/metodos_analiticos/proyecto/datos/TSp.RDS") 
TSp<-readRDS(file = "/home/urielmtzsa/itam/semestre4/metodos_analiticos/proyecto/datos/TSp.RDS") 


i<-0
for(c in unique(sales_m$country)){
  for(s in unique(sales$store)){
    for(p in unique(sales$product)){
      ajuste <- TSp[[paste(c,s,p,sep = "_")]]
      dims <- ajuste$state.contributions |> dim()
      tiempo <- dims[3]
      contribuciones_p <- map(1:tiempo, ~ ajuste$state.contributions[,,.x] |> 
        as_tibble() |> mutate(t = .x)) |> bind_rows() |> 
        pivot_longer(trend:seasonal.12.1, values_to = "value", names_to = "comp") |> 
        group_by(t, comp) |> 
        summarise(media = mean(value), q5 = quantile(value, 0.05),
                  q95 = quantile(value, 0.95), .groups = "drop") |>
        group_by(t) |>
        summarise(media_producto = sum(media)) |>
        mutate(p = paste(c,s,p,sep = "_"))
        pred <- predict(ajuste, horizon = 24,burn=2000)$mean
        contribuciones_p<-rbind(contribuciones_p,tibble(t = 49:72, media_producto = pred,p = paste(c,s,p,sep = "_")))
        if(i==0){contribuciones_p_producto<-contribuciones_p}else{contribuciones_p_producto<-rbind(contribuciones_p_producto,contribuciones_p)}
        i<-i+1
    }
  }
}

pp1<-contribuciones_producto_tbl |>
  separate(p, c("country","store","product"),sep="_") |>
  filter(country == "Poland") |>
  filter(store == "KaggleMart") 

pp2<-contribuciones_store_tbl |>
  separate(p, c("country","store"),sep="_") |>
  filter(country == "Poland") |>
  filter(store == "KaggleMart") 

pp3<-contribuciones_p_producto |>
  separate(p, c("country","store","product"),sep="_") |>
  filter(country == "Poland") |>
  filter(store == "KaggleMart") |>
  group_by(t,country,store) |>
  mutate(p_all = sum(media_producto)) |>
  mutate(media_producto = media_producto/p_all)

pp4<-merge(pp2,pp3, by =  c("t","country","store"),all=TRUE) |> 
  tibble() |>
  mutate(media_producto=round(media_producto*media_store,0))

pp4<-merge(pp4,pp1, by =  c("t","country","store","product"),all=TRUE,suffixes = c("_proportion","_serie")) |> 
  tibble() 

ggplot(pp4)+ 
  geom_line(aes(x = t, y = media_producto_serie/1000,colour = "Producto") ) +
  geom_line(aes(x = t, y = media_producto_proportion/1000, colour = "Store"),linetype = "dashed",size = 1) +
  labs(x = "t", y = "sales (miles)", title = "Poland & KaggleMart") +
  scale_color_manual(name = "Serie Jerárquica", values = c("Producto" = "red", "Store" = "darkblue")) + 
  geom_vline(xintercept = 48, linetype="dotted", color = "black", size=1.5) +
  facet_wrap(vars(product), nrow = 2, scales = "free_y")

ppp1<-contribuciones_producto_tbl |>
  separate(p, c("country","store","product"),sep="_") |>
  filter(country == "Germany") |>
  filter(store == "KaggleRama") 

ppp2<-contribuciones_store_tbl |>
  separate(p, c("country","store"),sep="_") |>
  filter(country == "Germany") |>
  filter(store == "KaggleRama") 

ppp3<-contribuciones_p_producto |>
  separate(p, c("country","store","product"),sep="_") |>
  filter(country == "Germany") |>
  filter(store == "KaggleRama") |>
  group_by(t,country,store) |>
  mutate(p_all = sum(media_producto)) |>
  mutate(media_producto = media_producto/p_all)

ppp4<-merge(ppp2,ppp3, by =  c("t","country","store"),all=TRUE) |> 
  tibble() |>
  mutate(media_producto=round(media_producto*media_store,0))

ppp4<-merge(ppp4,ppp1, by =  c("t","country","store","product"),all=TRUE,suffixes = c("_proportion","_serie")) |> 
  tibble() 
