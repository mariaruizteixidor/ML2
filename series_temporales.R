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

mape <- function(actual,pred){
  mape <- mean(abs((actual - pred)/actual))*100
  return (mape)
}

#check null values
(cols_withNa <- apply(df_ree, 2, function(x) sum(is.na(x))))

df_demanda <- df_ree %>%
  filter(subcategory == "Demanda en b.c.")

df_demanda$datetime <- as.POSIXct(df_demanda$datetime)
df_demanda$date <- as.Date(as.character(df_demanda$datetime)) #, format="%d/%m/%Y", origin = "2018-01-01"

# dividir TRAIN - TEST
# Train: enero 2018- enero 2020
df_demanda_train = df_demanda[df_demanda$date < '2020-02-01',]
# Test: febrero 2020
df_demanda_test = df_demanda[(df_demanda$date > '2020-01-31') & (df_demanda$date <= '2020-02-10'),] #2020-03-01

df_demanda = df_demanda[(df_demanda$date < '2020-02-10'),] ##2020-03-01

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
#df_demanda_train %>% ACF(difference(energy, 1)) %>% autoplot()
# Mejor observar el ACF y PACF sin la diferenciación primero

# el pico significativo en el retardo 7 del ACF sugiere un componente MA(1) estacional
# Vemos que hay unos picos estacionales cada 7 días -> componente estacional. 
# El pico significativo en el retardo 1 del ACF sugiere por lo menos una componente MA(2) no estacional 
df_demanda_train %>% ACF(energy) %>% autoplot()

#Autocorrelación parcial

# correlación que resulta después de eliminar el efecto de cualquier correlación en retardos más cortos
# autocorrelación entre los instantes t y t + k condicionada a  los valores que toma la serie en los instantes t + 1, t + 2, ... , t + k − 1 .
#df_demanda_train %>% PACF(difference(energy, 1)) %>% autoplot()

# Mejor observar el ACF y PACF sin la diferenciación primero
#Se continúa observando picos estacionales a los 7 días 
# Sigue un comportamiento parecido al gráfico anterior: El pico significativo en el retardo 1 del ACF sugiere por lo menos una componente MA(2) no estacional 
#un pico en la primera componente, sugiriendo una componente autoregresiva por lo menos de valor 1.
df_demanda_train %>% PACF(energy) %>% autoplot()

df_demanda1_diff <- difference(df_demanda_train$energy, 7)

# ARIMA
#Ver qué parámetros añadir p, d, q 
# Como hemos observado, el modelo tendrá que ser estacional, por lo que probamos a ajustar un modelo SARIMA.

# SARIMA ---------------------------------------------------------------------------------------------------------------------------------
# Ajuste 1
# Probamos ajustando un modelo con componentes de 
fit <- df_demanda_train %>%
  model(arima = ARIMA(energy ~ pdq(0,0,2) + PDQ(0,0,2))) #pdq(0,0,2) + PDQ(0,0,2)

report(fit)
gg_arma(fit)
# Si mostramos los residuos vemos que aún no es un ruido blanco, aparecen picos que habría que evitar.
# Si observamos el gráfico ACF vemos que aún hay picos en retardos 14, 21, 28.. que hay que ajustar.
# Si observamos el gráfico PACF vemos que hay retardos en los mismos retardos con picos prácticamente.
fit %>% my_tsresiduals()

# Forecast
forecast_1 =  forecast(fit, h=10) 

# PLot forecast
#forecast %>% autoplot(df_demanda_train)
forecast_1 %>% autoplot(df_demanda%>% filter(date >= "2020-01-01"))

# Residuals
mape(df_demanda_test$energy, forecast_1$energy) 
accuracy(forecast_1, df_demanda_test) #$energy

# Ajuste 2
# Probamos ajustando valores para modelar las componentes estacionales.
fit2 <- df_demanda_train %>%
  model(arima = ARIMA(energy ~ pdq(1,0,2) + PDQ(0,2,3))) #pdq(0,0,2) + PDQ(0,0,2)

report(fit2)
gg_arma(fit2)
fit2 %>% my_tsresiduals()

# Forecast
forecast_2 =  forecast(fit2, h=10) 

# PLot forecast
#forecast %>% autoplot(df_demanda_train)
forecast_2 %>% autoplot(df_demanda%>% filter(date >= "2020-01-01"))

# Residuals
mape(forecast_2$energy, df_demanda_test$energy) 
accuracy(forecast_2$energy, df_demanda_test$energy)

# Ajuste 3
# Primero probamos con un modelo sin ajustar, automático:
fit3 <- df_demanda_train %>%
  model(arima = ARIMA(energy))

# Vemos que ajusta un modelo ARIMA(2,0,3)(1,1,0)[7]. Es decir, detecta bien la estacionalidad semanal.
report(fit3)
gg_arma(fit3)
#Si visualizamos los residuos, vemos que aún hay picos que hacen que no sea ruido blanco todavía y un pico en el ACF en el retardo 14.
fit3 %>% my_tsresiduals()

# Forecast
forecast_3 =  forecast(fit3, h=10) 

# PLot forecast
#forecast %>% autoplot(df_demanda_train)
forecast_3 %>% autoplot(df_demanda%>% filter(date >= "2020-01-01"))

# Residuals
mape(df_demanda_test$energy, forecast_3$energy) 
accuracy(forecast_3, df_demanda_test)

# MODELO DUMMY  ---------------------------------------------------------------------------

fit_prueba <- df_demanda_train %>%
  model(
    ets = ETS(energy),
    arima = ARIMA(energy),
    snaive = SNAIVE(energy)) 

fc <- fit_prueba %>% forecast(h = 10)

fc %>%
  filter(date >= "2020-01-01") %>% autoplot(df_demanda %>% filter(date >= "2020-01-01"))

accuracy(fc, df_demanda_test)

#EXTRA....................................................................................

# MODELO AJUSTADO MANUALMENTE ---------------------------------------------------------------------------------------------------------
#Mejor ARIMA
model1 = Arima(df_demanda_train$energy,  order = c (1,1,0)) #, method="CSS"
model1
summary(model1)
coeftest(model1) 

# Raíces complejas dentro del círculo
# Se puede comprobar que las tres raices de la parte autoregresiva están dentro del círculo unidad, por tanto el proceso es estacionario. 
#gg_arma(model1)

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


# FUNCIÓN MY RESIDUALS

my_tsresiduals <- function(data, ...){
  if(!fabletools::is_mable(data)){
    abort("gg_tsresiduals() must be used with a mable containing only one model.")
  }
  
  data <- stats::residuals(data)
  if(n_keys(data) > 1){
    abort("gg_tsresiduals() must be used with a mable containing only one model.")
  }
  
  gg_tsdisplay(data, !!sym(".resid"), plot_type = "partial", ...)
}

#' @export
`+.gg_tsensemble` <- function(e1, e2){
  e1[[1]] <- e1[[1]] + e2
  e1
}

#' @export
print.gg_tsensemble <- function(x, ...){
  print(x[[1]], vp = grid::viewport(layout.pos.row = c(1, 1), layout.pos.col = c(1, 2)))
  print(x[[2]], vp = grid::viewport(layout.pos.row = 2, layout.pos.col = 1))
  print(x[[3]], vp = grid::viewport(layout.pos.row = 2, layout.pos.col = 2))
}

