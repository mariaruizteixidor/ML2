library(tidyverse)
library(tsibble)
library(fable)
library(feasts)
library(tsibbledata)
library(normtest)

setwd("~/Escritorio/MASTER/CUATRI3/MACHINE_LEARNING2/ML2/PRACTICA")
df_ree <- read.csv("~/Escritorio/MASTER/CUATRI3/MACHINE_LEARNING2/PRACTICA/ML2/ree.csv")

#df_ree$datetime <- strptime(x = as.character(df_ree$datetime),
#                                format = "%d/%m/%Y %H:%M")
df_ree$id <- NULL
df_ree$date = substr(df_ree$datetime,1,10)

#check null values
(cols_withNa <- apply(df_ree, 2, function(x) sum(is.na(x))))

df_demanda <- df_ree %>%
  filter(subcategory == "Demanda en b.c.")

#df_demanda$datetime <- as.POSIXct(df_demanda$datetime)
df_demanda$date <- as.Date(df_demanda$datetime, format="%d/%m/%Y")

# Transform into a tibble object
df_demanda1 <- df_demanda %>%
  as_tsibble(index = date) #key = c(energy), index = datetime

# plot series temporales
df_demanda1 %>%
  autoplot(energy) +
  labs(title = "", subtitle = "") 

# Correlograma
df_demanda1 %>% ACF(energy) %>% autoplot()

# Descomposición clásica
df_demanda1 %>%
  model(classical_decomposition(energy, type = "additive")) %>%
  components() %>%
  autoplot() + xlab(" ") +
  ggtitle(" ")

# Descomposición X11
x11_dcmp <- df_demanda1 %>%
  model(x11 = feasts:::X11(energy, type = "additive")) %>%
  components()

autoplot(x11_dcmp) + xlab(" ") +
  ggtitle(" ")
