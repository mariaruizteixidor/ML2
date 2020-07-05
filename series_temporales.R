library(tidyverse)
library(tsibble)
library(fable)
library(feasts)
library(tsibbledata)
library(normtest)
library(lmtest)
library(forecast)

setwd("~/Escritorio/MASTER/CUATRI3/MACHINE_LEARNING2/PRACTICA/ML2")
df_ree <- read.csv("~/Escritorio/MASTER/CUATRI3/MACHINE_LEARNING2/PRACTICA/ML2/ree.csv")

#df_ree$datetime <- strptime(x = as.character(df_ree$datetime),
#                                format = "%d/%m/%Y %H:%M")
df_ree$id <- NULL
df_ree$date = substr(df_ree$datetime,1,10)

#check null values
(cols_withNa <- apply(df_ree, 2, function(x) sum(is.na(x))))

df_demanda <- df_ree %>%
  filter(subcategory == "Demanda en b.c.")

df_demanda$datetime <- as.POSIXct(df_demanda$datetime)
df_demanda$date <- as.Date(df_demanda$datetime, format="%d/%m/%Y")

# Transform into a tibble object
df_demanda1 <- df_demanda %>%
  as_tsibble(index = date) #key = c(energy), index = datetime

# plot series temporales
df_demanda1 %>%
  autoplot(energy) +
  labs(title = "", subtitle = "") 

# Descomposición clásica
#aditivo 
df_demanda1 %>%
  model(classical_decomposition(energy, type = "additive")) %>%
  components() %>%
  autoplot() + xlab(" ") +
  ggtitle(" ")

demanda_decomposed = classical_decomposition(energy, type = "additive")

#multiplicativo
df_demanda1 %>%
  model(classical_decomposition(energy, type = "multiplicative")) %>%
  components() %>%
  autoplot() + xlab(" ") +
  ggtitle(" ")

# Diferenciación
# Se observa que tras la diferenciación la serie pierde la tendencia, siendo claramente estacionaria en media.
# yt = xt - x(t-1)
df_demanda1 %>% autoplot( difference(energy)) +
  xlab("  ") + ylab(" ") +
  ggtitle("  ")

# Una forma de determinar más objetivamente si es necesario diferenciar es utilizar una prueba de raíz unitaria. 
# En este contraste, la hipótesis nula es que los datos son estacionarios, y buscamos pruebas de que la hipótesis nula es falsa. En consecuencia, pequeños p-valores (por ejemplo, menos de 0,05) sugieren que es necesario diferenciar.
df_demanda1 %>%
  features(energy, unitroot_kpss)
#El p-valor es menor que 0.05, lo que indica que la hipótesis nula es rechazada.

df_demanda1 %>%
  mutate(diff_close = difference(energy)) %>%
  features(diff_close, unitroot_kpss)
#Esta vez, el estadístico de la prueba es diminuto, el p-valor es mayor que 0.05, y está dentro del rango que esperaríamos para los datos estacionarios. 

#Comprobar si existen autocorrelaciones significaticas:
df_demanda1 %>%
  mutate(diff_close = difference(energy)) %>%
  features(diff_close, ljung_box, lag = 10)

# MODELOS
# es posible utilizar las gráfica de ACF y PACF, para determinar los valores apropiados para p y q.
# Correlograma
# Gráfico en el que representamos los valores de la función de autocorrelación empírica r k contra los retardos k = 1, 2, ... , M donde típicamente M ≪ N .
# Vemos que  tiene muchos componentes significativas para algún retardo.
df_demanda1 %>% ACF(difference(energy, 7)) %>% autoplot()

#Autocorrelación parcial
# Con o sin diferenciación???

# correlación que resulta después de eliminar el efecto de cualquier correlación en retardos más cortos
# autocorrelación entre los instantes t y t + k condicionada a  los valores que toma la serie en los instantes t + 1, t + 2, ... , t + k − 1 .
df_demanda1 %>% PACF(difference(energy, 7)) %>% autoplot()

df_demanda1_diff <- difference(df_demanda1$energy, 7)

# ARIMA
#Ver qué parámetros añadir p, d, q 
# componente no estacional: 2 (con diferenciación)
# componente estacional: 7 (con diferenciación)
model1 = arima (df_demanda1$energy,  order = c (2,7,0), method="CSS")
model1

coeftest(model1) 

# Residuals
checkresiduals(model1)

# Forecast
autoplot(forecast(model1))

# 2 
fit2 <- df_demanda1 %>%
  model(arima = ARIMA(energy ~ pdq(6,0,0) + PDQ(0,0,0)))

report(fit2)

# Raíces complejas dentro del círculo
# Se puede comprobar que las tres raices de la parte autoregresiva están dentro del círculo unidad, por tanto el proceso es estacionario. 
gg_arma(fit2)

# SARIMA
fit <- df_demanda1 %>%
  model(arima = ARIMA(energy ~ pdq(0,1,1) + PDQ(0,1,1)))

# ver errores


#predicción
fit %>% forecast(h=12) %>% autoplot(df_demanda1)
