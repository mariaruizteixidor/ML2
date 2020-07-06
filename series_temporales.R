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

# dividir TRAIN - TEST
df_demanda_train = df_demanda[df_demanda$date < '2020-02-01',]
df_demanda_test = df_demanda[(df_demanda$date >= '2020-02-01') & (df_demanda$date < '2020-03-01'),]

# Transform into a tibble object
df_demanda_train <- df_demanda_train %>%
  as_tsibble(index = date) #key = c(energy), index = datetime

df_demanda_test <- df_demanda_test %>%
  as_tsibble(index = date) #key = c(energy), index = datetime# Descomposición clásica

df_demanda <- df_demanda %>%
  as_tsibble(index = date) #key = c(energy), index = datetime# Descomposición clásica

# plot series temporales
df_demanda_train %>%
  autoplot(energy) +
  labs(title = "", subtitle = "") 

#aditivo 
df_demanda_train %>%
  model(classical_decomposition(energy, type = "additive")) %>%
  components() %>%
  autoplot() + xlab(" ") +
  ggtitle(" ")

demanda_decomposed = classical_decomposition(energy, type = "additive")

#multiplicativo
df_demanda_train %>%
  model(classical_decomposition(energy, type = "multiplicative")) %>%
  components() %>%
  autoplot() + xlab(" ") +
  ggtitle(" ")

# Diferenciación
# Se observa que tras la diferenciación la serie pierde la tendencia, siendo claramente estacionaria en media.
# yt = xt - x(t-1)
df_demanda_train %>% autoplot( difference(energy)) +
  xlab("  ") + ylab(" ") +
  ggtitle("  ")

# Una forma de determinar más objetivamente si es necesario diferenciar es utilizar una prueba de raíz unitaria. 
# En este contraste, la hipótesis nula es que los datos son estacionarios, y buscamos pruebas de que la hipótesis nula es falsa. En consecuencia, pequeños p-valores (por ejemplo, menos de 0,05) sugieren que es necesario diferenciar.
df_demanda_train %>%
  features(energy, unitroot_kpss)
#El p-valor es menor que 0.05, lo que indica que la hipótesis nula es rechazada.

df_demanda_train %>%
  mutate(diff_close = difference(energy)) %>%
  features(diff_close, unitroot_kpss)
#Esta vez, el estadístico de la prueba es diminuto, el p-valor es mayor que 0.05, y está dentro del rango que esperaríamos para los datos estacionarios. 

#Comprobar si existen autocorrelaciones significaticas:
df_demanda_train %>%
  mutate(diff_close = difference(energy)) %>%
  features(diff_close, ljung_box, lag = 10)

# MODELOS
# es posible utilizar las gráfica de ACF y PACF, para determinar los valores apropiados para p y q.
# Correlograma
# Gráfico en el que representamos los valores de la función de autocorrelación empírica r k contra los retardos k = 1, 2, ... , M donde típicamente M ≪ N .
# Vemos que  tiene muchos componentes significativas para algún retardo.
df_demanda_train %>% ACF(difference(energy, 7)) %>% autoplot()

#Autocorrelación parcial
# Con o sin diferenciación???

# correlación que resulta después de eliminar el efecto de cualquier correlación en retardos más cortos
# autocorrelación entre los instantes t y t + k condicionada a  los valores que toma la serie en los instantes t + 1, t + 2, ... , t + k − 1 .
df_demanda_train %>% PACF(difference(energy, 7)) %>% autoplot()

df_demanda1_diff <- difference(df_demanda_train$energy, 7)

# ARIMA
#Ver qué parámetros añadir p, d, q 
# componente no estacional: 2 (con diferenciación)
# componente estacional: 7 (con diferenciación)

# MODELO AJUSTADO MANUALMENTE ---------------------------------------------------------------------------------------------------------
model1 = Arima(df_demanda_train$energy,  order = c (5,1,0)) #, method="CSS"
model1
summary(model1)
coeftest(model1) 

# Raíces complejas dentro del círculo
# Se puede comprobar que las tres raices de la parte autoregresiva están dentro del círculo unidad, por tanto el proceso es estacionario. 
gg_arma(model1)

# Representar serie + predicción data test (modelo auto)
#prediccion = forecast(model_auto, h = 50) #df_demanda_test$energy,
prediction_model1<-predict(model1, n.ahead = 30)$pred
plot(df_demanda_train$energy,col="grey",lwd=1.5,ylab="Demanda energética train + forecast", type = 'l') #, xlim=c(0,700)
lines(prediction_model1, col="blue",lwd=1.5)
time_series_test = ts(df_demanda_test$energy, start = 763, end = 792)
lines(time_series_test, col="green",lwd=1.5)

# Residuals
checkresiduals(prediction_model1)

accuracy(prediction_model1, df_demanda_test$energy) 

# MODELO AJUSTADO AUTOMÁTICAMENTE ---------------------------------------------------------------------------------------------------
model_auto = auto.arima(df_demanda_train$energy)
model_auto
summary(model_auto)

# Representar serie + predicción data test (modelo auto)
#prediccion = forecast(model_auto, h = 50) #df_demanda_test$energy,
prediction_auto<-predict(model_auto, n.ahead = 30)$pred
plot(df_demanda_train$energy,col="grey",lwd=1.5,ylab="Demanda energética train + forecast", type = 'l') #, xlim=c(0,529)
lines(prediction_auto, col="blue",lwd=1.5)
time_series_test = ts(df_demanda_test$energy, start = 763, end = 792)
lines(time_series_test, col="green",lwd=1.5)

# errores
checkresiduals(prediction_auto)

accuracy(prediction_auto, df_demanda_test$energy)

# SARIMA ---------------------------------------------------------------------------------------------------------------------------------
fit <- df_demanda_train %>%
  model(arima = ARIMA(energy ~ pdq(0,1,1) + PDQ(0,1,1)))

#predicción
fit %>% forecast(h=12) %>% autoplot(df_demanda_train)

# ver errores