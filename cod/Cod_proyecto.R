
#Librerias:----
##Básicas:----
library(readr)
library(dplyr)
library(lubridate)
library(ggplot2)
library(tidyr)

##Extra: ----
library(corrplot)
library(ggrepel)



#Funciones: ----

###PENDIENTES: CONTROL ERRORES ENTRADA: ----

##(i) ----

#Funcion recibe vector precios, devuelve vector retornos logarítmicos (1er elemento NA):
  log.retornos <- function(precios){
    
    #NA principio iguala tamaño vectores
    #length(diff(log(precios))) = length(precios) - 1
    
    c(NA, diff(log(precios)))
  }


##(ii) ----

#Funcion recibe vector retornos, devuelve valor volatilidad anual:
calcular.volatilidad.anual <- function(retornos){
  
  #Definimos n como el número de observaciones usadas:
  n = length(retornos)
  
  #Calcula desviación estándar:
  s = sqrt( 1/(n - 1) * sum( (retornos - mean(retornos, na.rm = T))^2, na.rm = T ))
  
  #Calcula y retorna la volatilidad:
  volatilidad = s*sqrt(252)
  
 return(volatilidad)

}


##(iii) ----


proyectar.accion <- function(s0, mu, vol, dt, z){
  
#Tipos esperados:
  #vol: escalar
  #mu: escalar
  #t: escalar
  #s0: vector o escalar
  #z: vector o escalar
  
St = s0 * exp( (mu - (vol)^2/2)*dt + vol*sqrt(dt)*z )

return(St)
}





#Lectura data: ----

stock.price.raw <- read_csv("data/master_stock_data.csv")



#Revisión preliminar data: ----

str(stock.price.raw) 
problems(stock.price.raw)
dim(stock.price.raw) 
head(stock.price.raw) 
summary(stock.price.raw)

#Empresas etiquetas:
unique(stock.price.raw$Ticker)



#Depuración y filtrado de la data: ----


#Filtramos data para un intervalo de tiempo con los últimos 10 años
#y empresas con más de 2000 observaciones o días:

stock.price.dep <- stock.price.raw %>% 
  filter(Date >= as_date("2016-01-01")) %>% 
  group_by(Ticker)  %>% 
  filter(n() >= 2000) %>% 
  ungroup() 


## Criterios de selección muestra (empresas) usada proyecto: ----

#Creamos criterios clasificatorios selección muestra (Volumen medio transacción y Volatilidad):

#Volumen medio de transacciones en millones de dolares:
stock.volumen <- stock.price.dep %>%
  summarise(Volumen.medio = mean(Volume*`Adj Close`, na.rm = T)/1000000 ,.by = Ticker) %>% 
  arrange(desc(Volumen.medio)) %>%  
  mutate(
    tercil.volumen = ntile(Volumen.medio, 3)
  ) %>%
  mutate(
    Clasificacion.vol = case_when(
      tercil.volumen == 1 ~ "Bajo volumen",
      tercil.volumen == 2 ~ "Medio volumen",
      tercil.volumen == 3 ~ "Alto volumen"
     )
    ) %>% 
  select(-tercil.volumen)

#Volatilidad historica últimos 3 años (trading days):
stock.volatilidad <- stock.price.dep %>%
  summarise(Volatilidad = calcular.volatilidad.anual(tail(log.retornos(`Adj Close`), 756)), .by = Ticker) %>%
              arrange(desc(Volatilidad)) %>% 
  mutate(tercil.volatilidad = ntile(Volatilidad, 3)) %>%
  mutate(
    Clasificacion.sigma  = case_when(
      tercil.volatilidad  == 1 ~ "Baja σ",
      tercil.volatilidad  == 2 ~ "Media σ",
      tercil.volatilidad  == 3 ~ "Alta σ"
    )
  ) %>% 
  select(-tercil.volatilidad)


#Usando la clasificaciones, preparamos la seleccion de la muestra para el proyecto,
#uniendo ambas datas clasificatorias:

seleccion <- stock.volumen %>% 
  left_join(stock.volatilidad, 
  by = "Ticker") %>%
  mutate(
    Grupo = paste(
      Clasificacion.vol,
      Clasificacion.sigma,
      sep = " | "
    )
  )

#Cuantas empresas en cada grupo tiene seleccion:
seleccion %>%
  count(Grupo)


#Creamos la muestra de la empresas que vamos a utilizar seleccionando el primer
#y el último elemento de cada grupo (ordenados por volumen medio de transacción):

muestra <- seleccion %>%
  group_by(Grupo) %>%
  slice(c(1, n())) %>%
  ungroup() %>% 
  arrange(desc(Volumen.medio)) %>% 
  select(-c("Volumen.medio", "Volatilidad"))

#Muestra alterna ordenada x volatilidad:
muestra.volt <- seleccion %>%
  group_by(Grupo) %>%
  slice(c(1, n())) %>%
  ungroup() %>% 
  arrange(desc(Volatilidad)) %>% 
  select(-c("Volumen.medio", "Volatilidad"))

#Nombres empresas de la muestra:

muestra$Ticker

#Verifiación muestra completa:

muestra %>%
  count(Clasificacion.vol, Clasificacion.sigma)

##Data final filtrada con muestra: ----

stock.price.dep <- stock.price.dep %>% 
  filter(Ticker %in% muestra$Ticker)




#Información descriptiva data y tablas resumen de contenidos: ----


# Revisamos si la data quedo con NA's:
stock.price.dep %>% summarise(across(everything(), \(x)sum(is.na(x)))) 
#No tiene

#Filas o trading days por compañia:
stock.price.dep %>% count(Ticker)

#Rango de tiempo:
range(stock.price.dep$Date)

#Maximo dias de diferencia entre una fecha y otra:
max(diff(stock.price.dep$Date))

#Revisamos las fechas de las empresas que no coincidan:
stock.price.dep %>% 
  count(Date) %>%
  filter(n != 18)
#Hay dos empresas con una fecha adicional: "2026-08-28"
stock.price.dep %>% filter(Date == as_date("2026-08-28"))
    #Son Apple y Tesla.

#Para efectos prácticos la borramos (igualamos fechas tdas empresas):
stock.price.dep <- stock.price.dep %>% filter(Date != as_date("2026-08-28"))


#Trading days por año (igual para tdas compañias):
trng.days <- stock.price.dep %>%
  mutate(Anio = year(Date)) %>%
  filter(Ticker == "AAPL") %>% 
  count(Anio, name = "Dias")

#Media de trading days (sin 2026):
trng.days$Dias[1:10] %>% mean()  # 251.4


# #Tabla resumen rango posición de cada empresa:
# stock.price.dep %>% summarise(inicio = min(t),
#                      final = max(t),
#                      .by = Ticker)


#Tabla resumen num. Fechas con dividendos por empresa:

stock.price.dep %>% summarise(Fechas.pago = sum(Dividends != 0),
                      .by = Ticker)




#Relaciones entre empresas: ----

#Estudiamos las correlaciones entre los retornos de las acciones de cada empresa
#en la muestra:

#Creamos tibble de los retornos logaritmicos de cada empresa
stock.returns <- stock.price.dep %>% 
  group_by(Ticker) %>% 
  mutate(Log.Retornos = log.retornos(`Adj Close`)) %>% 
  select(Date, Ticker, Log.Retornos) %>% 
  ungroup()

#Acomodamos los retornos por cada fecha para cada empresa:
retornos.wide <- stock.returns %>%
  pivot_wider(
    names_from = Ticker,
    values_from = Log.Retornos
  )

#Creamos la matriz de correlaciones:
matriz.cor <- retornos.wide %>%
  select(-Date) %>%
  cor(use = "pairwise.complete.obs")
#Matriz 18x18 diagonal todos son 1:

#Llenamos la diagonal con NAs
diag(matriz.cor) <- NA

#Encontramos la correlación máxima de cada columna/empresa:
cor.maximas <- apply(matriz.cor, 2, max, na.rm = TRUE)


#Encontramos la mínima correlación de cada columna/empresa:
cor.min <- apply(matriz.cor, 2, min, na.rm = TRUE) %>% sort()

#Caculamos la correlación media de cada empresa con las otras:
cor.media <- apply(
  matriz.cor, 2, mean,
  na.rm = TRUE
) %>% sort(decreasing = TRUE)















#Graficos: ----
# ggplot(stock.price.dep[1:252, ], aes(t, Log_Retornos)) +
#   geom_line(color = "steelblue") +
#   geom_hline(yintercept = 0,
#              linetype = "dashed",
#              color = "red") +
#   labs(
#     title = "Serie temporal de log-retornos",
#     x = "Observación",
#     y = "Log-retorno"
#   ) +
#   theme_minimal()


##Graficos serie histórica precio: ----

#Ordenados por volatilidad:


#1era parte:
empresas.1 <- muestra$Ticker[1:9]

# Datos para el gráfico
datos.plot <- stock.price.dep %>%
  filter(Ticker %in% empresas.1) %>%
  arrange(Ticker, Date) %>%
  group_by(Ticker) %>%
  mutate(
    Base100 = (`Adj Close` / first(`Adj Close`)) * 100
  ) %>%
  ungroup()

# Última observación de cada empresa
etiquetas <- datos.plot %>%
  group_by(Ticker) %>%
  slice_max(Date, n = 1) %>%
  ungroup() %>%
  arrange(desc(Base100))

ggplot(
  datos.plot,
  aes(
    x = Date,
    y =`Adj Close`,
    color = Ticker
  )
) +
  
  geom_line(
    linewidth = 0.8
  ) +
  
  geom_text_repel(
    data = etiquetas,
    aes(
      label = Ticker
    ),
    direction = "y",
    hjust = 0,
    nudge_x = 300,
    segment.color = "grey50",
    show.legend = FALSE
  ) +
  
  scale_x_date(
    breaks = seq(
      as.Date("2016-01-01"),
      as.Date("2026-01-01"),
      by = "2 years"
    ),
    date_labels = "%Y",
    expand = expansion(mult = c(0.02, 0.08)),
  ) +
  
  labs(
    title = "Evolución histórica de precios ajustados",
    subtitle = "Grupo 1 de empresas seleccionadas",
    x = "Año",
    y = "Precio acción USD"
  ) +
  
  theme_minimal(base_size = 13) +
  
  theme(
    legend.position = "none",
    plot.title = element_text(
      face = "bold",
      hjust = 0.5
    ),
    plot.subtitle = element_text(
      hjust = 0.5
    )
  )

# 
# stock.price.dep %>% 
#   filter(Ticker %in% muestra$Ticker[1:9]) %>% 
#   summarise(Maximo = max(`Adj Close`, na.rm = T),.by = Ticker) %>% 
#   arrange(desc(Maximo))
# 
# 
# stock.price.dep %>% 
#   filter(Ticker %in% muestra.volt$Ticker[1:9]) %>% 
#   summarise(Maximo = max(`Adj Close`, na.rm = T),.by = Ticker) %>% 
#   arrange(desc(Maximo))
#

#Sda parte empresas:
empresas.2 <- muestra$Ticker[10:18]

# Datos para el gráfico
datos.plot.2 <- stock.price.dep %>%
  filter(Ticker %in% empresas.2) %>%
  arrange(Ticker, Date)

# Última observación de cada empresa
etiquetas.2 <- datos.plot.2 %>%
  group_by(Ticker) %>%
  slice_max(Date, n = 1) %>%
  ungroup()

ggplot(
  datos.plot.2,
  aes(
    x = Date,
    y =`Adj Close`,
    color = Ticker
  )
) +
  
  geom_line(
    linewidth = 0.8
  ) +
  
  geom_text_repel(
    data = etiquetas.2,
    aes(
      label = Ticker
    ),
    direction = "y",
    hjust = 0,
    nudge_x = 300,
    segment.color = "grey50",
    show.legend = FALSE
  ) +
  
  scale_x_date(
    breaks = seq(
      as.Date("2016-01-01"),
      as.Date("2026-01-01"),
      by = "2 years"
    ),
    date_labels = "%Y",
    expand = expansion(mult = c(0.02, 0.08)),
  ) +
  
  labs(
    title = "Evolución histórica de precios ajustados",
    subtitle = "Grupo 2 de empresas seleccionadas",
    x = "Año",
    y = "Precio acción USD"
  ) +
  
  theme_minimal(base_size = 13) +
  
  theme(
    legend.position = "none",
    plot.title = element_text(
      face = "bold",
      hjust = 0.5
    ),
    plot.subtitle = element_text(
      hjust = 0.5
    )
  )




muestra.volt$Ticker




##Grafico de calor con correlaciones empresas: ----

diag(matriz.cor) <- 1

corrplot(
  matriz.cor,
  method = "color",
  order = "hclust",
  hclust.method = "complete",
  addrect = 3,
  title = "Correlacion entre los rendimientos de las empresas", 
  tl.col = "#05235c",
  tl.cex = 0.9,
  rect.col = "#05235c",
  mar = c(0,0,2,0)
)


# Modelo GBM: ----

##(i) Parametros: ----

###(1) Rentabilidad esperada: ----

#Media de retornos cada empresa (últimos 10 años):

retornos.diarios <- stock.returns %>% summarise(r.medio = mean(Log.Retornos, na.rm = T),
                     .by = Ticker)

mu <- retornos.diarios %>% 
  left_join(stock.volatilidad %>% 
              select(-Clasificacion.sigma), 
              by = "Ticker") %>% 
  mutate(Retorno.esperado.anual = 252*r.medio + (Volatilidad)^2/2) %>% 
  select(Ticker, Retorno.esperado.anual)

###(2)Volatilidad histórica: ----

sigma <- stock.volatilidad %>% 
  filter(Ticker %in% muestra$Ticker) %>% 
  select(-c(Clasificacion.sigma), Volatilidad.anual = Volatilidad) %>% 
  arrange(Ticker)

###(3)Último precio registrado acción: ----
So <- stock.price.dep %>% 
  group_by(Ticker) %>% 
  slice_tail(n = 1) %>% 
  select(Ticker, `Adj Close`) %>% 
  rename(So = `Adj Close`)

#Verificamos que la fecha del último precio sea la misma para las 18:
stock.price.dep %>% 
  group_by(Ticker) %>% 
  slice_tail(n = 1) %>% 
  ungroup() %>% 
  group_by(Date) %>% 
  count( )


###Unimos las dos en una data con los párametros:
parametros <- mu %>% left_join(sigma, by = "Ticker") %>% 
             left_join(So, by = "Ticker") %>% 
             select(Ticker, So , everything())



##(ii)Shocks normales ----


#Creamos un data frame con shock normales aleatorios
#para la proyección de las 10000 trayectorias de 12 pasos (meses) 
#para el precio final de cada empresa:

#Guardamos los tickers de cada empresa:
shocks.normales <- stock.price.dep %>%
  group_by(Ticker) %>%
  slice_tail(n = 1) %>%
  ungroup() %>%
  select(c(Ticker))

#Creamos 10000 filas de cada ticker:
shocks.normales <- shocks.normales[rep(seq_len(nrow(shocks.normales)), each = 10000), ]

#Creamos las columnas de los shocks normales para cada mes hasta el mes 12:
set.seed(309)
#Creamos las columnas por cada 
for(i in 1:12){
  shocks.normales <- shocks.normales %>% 
    mutate(!!paste0("Z.", i) := rnorm(180000)) 
}

#Revisamos si la data genera NAs:
shocks.normales %>% summarise(across(everything(), \(x)sum(is.na(x))))
 #No lo hace

##(iii)Proyecciones ----

#Ahora creamos la data con la primer proyección de precios (columna: mes 1):
proyecciones <- shocks.normales %>%
  left_join(parametros, by = "Ticker") %>%
  mutate(Mes.1 = proyectar.accion(
    s0 = So,
    mu = Retorno.esperado.anual,
    vol = Volatilidad.anual,
    dt = 1 / 12,
    z = Z.1
  ))
  
#Creamos demás columnas para el resto de meses:
for(i in 2:12){
  
  proyecciones[[paste0("Mes.", i)]] <-
    proyectar.accion(
      s0 = proyecciones[[paste0("Mes.", i - 1)]],
      mu = proyecciones$Retorno.esperado.anual,
      vol = proyecciones$Volatilidad.anual,
      dt = 1/12,
      z = proyecciones[[paste0("Z.", i)]]
    )
  
}
 
#Guardamos la versión final de la proyección:
proyecciones.final <- proyecciones %>% 
  group_by(Ticker) %>% 
  mutate(Trayectoria = row_number()) %>% 
  ungroup() %>% 
  select(Ticker, Trayectoria, So, contains("Mes."))
  
  
            
#Carteras ----

##Inversión inicial ----

#Definimos el monto de inversión (en $):
inversion <- 102e03

#Pesos acciones:
#Defimos pesos iguales para todas las empresa. Es decir invertimos en cada empresa
#una proporción igual de nuestra inversión:

w <- 1/6
inversion.empresa <- inversion*w


##Construcción de las carteras ----

#Cada cartera tendrá delta acciones de 6 diferentes empresas, seleccionadas
#bajo distintos criterios:

#Cartera 1 (muy volatil):

cartera.1 <- muestra.volt %>% 
  filter(Clasificacion.sigma == "Alta σ") %>% 
  select(Ticker) %>% 
  left_join(parametros, by = "Ticker") %>% 
  select(-c("Retorno.esperado.anual", "Volatilidad.anual")) %>% 
  mutate(delta = inversion.empresa / So) 


Vo.1 <- sum(cartera.1$So * cartera.1$delta)
Total.acciones.1 <- sum(cartera.1$delta)

#Cartera 2 (poco volatil):

cartera.2 <- muestra.volt %>% 
  filter(Clasificacion.sigma == "Baja σ") %>% 
  select(Ticker) %>% 
  left_join(parametros, by = "Ticker") %>% 
  select(-c("Retorno.esperado.anual", "Volatilidad.anual")) %>% 
  mutate(delta = inversion.empresa / So) 


Vo.2 <- sum(cartera.2$So * cartera.2$delta)
Total.acciones.2 <- sum(cartera.2$delta)

#Cartera 3 (volatilidad media):

cartera.3 <- muestra.volt %>% 
  filter(Clasificacion.sigma == "Media σ") %>% 
  select(Ticker) %>% 
  left_join(parametros, by = "Ticker") %>% 
  select(-c("Retorno.esperado.anual", "Volatilidad.anual")) %>% 
  mutate(delta = inversion.empresa / So) 


Vo.3 <- sum(cartera.3$So * cartera.3$delta)
Total.acciones.3 <- sum(cartera.3$delta)

#Cartera 4 (no diversificada):

cartera.4 <- parametros %>% 
  filter(Ticker %in% c("AXP", "JPM", "SCHW", "PH", "URI", "LIN")) %>% 
  select(-c("Retorno.esperado.anual", "Volatilidad.anual")) %>% 
  mutate(delta = inversion.empresa / So)

Vo.4 <- sum(cartera.4$So * cartera.4$delta)
Total.acciones.4 <- sum(cartera.4$delta)

#Cartera 5 (diversificada):

cartera.5 <- parametros %>% 
  filter(Ticker %in% c("DUK", "TSLA", "LMT", "WBD", "RMD", "CMG")) %>% 
  select(-c("Retorno.esperado.anual", "Volatilidad.anual")) %>% 
  mutate(delta = inversion.empresa / So)

Vo.5 <- sum(cartera.5$So * cartera.5$delta)
Total.acciones.5 <- sum(cartera.5$delta)


#Verificamos que las carteras tengan el mismo valor:

inversion == c(Vo.1, Vo.2, Vo.3, Vo.4, Vo.5)

##Proyección carteras ----

#Cartera 1 (muy volatil):
cartera.1.proyectada <- cartera.1 %>% 
  left_join(
  proyecciones.final %>% select(c("Ticker", "Trayectoria", "Mes.12")), 
  by = "Ticker"
  ) %>% 
  select(Ticker, Trayectoria, delta, S1 = Mes.12)

cartera.1.proyectada %>% count(Ticker)

V1.1 <- cartera.1.proyectada %>%
  group_by(Trayectoria) %>%
  summarise(V1 = sum(delta * S1)) %>%
  pull(V1) %>% sort()


#Métricas del resultado:
valor.esperado.1 <- mean(V1.1, na.rm = T)
sd(V1.1)
quantile(V1.1, probs = c(0.05, 0.5, 0.95))

prob.perdida.1 <- length(V1.1[V1.1 < 102000])/length(V1.1)
SE.pp.1 <- sqrt(prob.perdida.1*(1-prob.perdida.1)/length(V1.1))
SE.Ve.1 <- sd(V1.1)/sqrt(length(V1.1))

IC.media.1 <- c(
  valor.esperado.1 - 1.96*SE.Ve.1,
  valor.esperado.1 + 1.96*SE.Ve.1
)

IC.prob.1 <-  c(
  prob.perdida.1 - 1.96*SE.pp.1,
  prob.perdida.1 + 1.96*SE.pp.1
)

#Cartera 2 (poco volatil):
cartera.2.proyectada <- cartera.2 %>% 
  left_join(
    proyecciones.final %>% select(c("Ticker", "Trayectoria", "Mes.12")), 
    by = "Ticker"
  ) %>% 
  select(Ticker, Trayectoria, delta, S1 = Mes.12)

cartera.2.proyectada %>% count(Ticker)

V1.2 <- cartera.2.proyectada %>%
  group_by(Trayectoria) %>%
  summarise(V1 = sum(delta * S1)) %>%
  pull(V1) %>% sort()

#Métricas del resultado:
valor.esperado.2 <- mean(V1.2, na.rm = T)

sd(V1.2)

quantile(V1.2, probs = c(0.05, 0.5, 0.95))

prob.perdida.2 <- length(V1.2[V1.2 < 102000])/length(V1.2)

SE.pp.2 <- sqrt(prob.perdida.2*(1-prob.perdida.2)/length(V1.2))

SE.Ve.2 <- sd(V1.2)/sqrt(length(V1.2))

IC.media.2 <- c(
  valor.esperado.2 - 1.96*SE.Ve.2,
  valor.esperado.2 + 1.96*SE.Ve.2
)

IC.prob.2 <-  c(
  prob.perdida.2 - 1.96*SE.pp.2,
  prob.perdida.2 + 1.96*SE.pp.2
)



#Cartera 3 (volatilidad media):
cartera.3.proyectada <- cartera.3 %>% 
  left_join(
    proyecciones.final %>% select(c("Ticker", "Trayectoria", "Mes.12")), 
    by = "Ticker"
  ) %>% 
  select(Ticker, Trayectoria, delta, S1 = Mes.12)

cartera.3.proyectada %>% count(Ticker)

V1.3 <- cartera.3.proyectada %>%
  group_by(Trayectoria) %>%
  summarise(V1 = sum(delta * S1)) %>%
  pull(V1)

#Métricas del resultado:
valor.esperado.3 <- mean(V1.3, na.rm = T)

sd(V1.3)

quantile(V1.3, probs = c(0.05, 0.5, 0.95))

prob.perdida.3 <- length(V1.3[V1.3 < 102000])/length(V1.3)

SE.pp.3 <- sqrt(prob.perdida.3*(1-prob.perdida.3)/length(V1.3))

SE.Ve.3 <- sd(V1.3)/sqrt(length(V1.3))

IC.media.3 <- c(
  valor.esperado.3 - 1.96*SE.Ve.3,
  valor.esperado.3 + 1.96*SE.Ve.3
)

IC.prob.3 <-  c(
  prob.perdida.3 - 1.96*SE.pp.3,
  prob.perdida.3 + 1.96*SE.pp.3
)

#Cartera 4 (no diversificada):
cartera.4.proyectada <- cartera.4 %>% 
  left_join(
    proyecciones.final %>% select(c("Ticker", "Trayectoria", "Mes.12")), 
    by = "Ticker"
  ) %>% 
  select(Ticker, Trayectoria, delta, S1 = Mes.12)

cartera.4.proyectada %>% count(Ticker)

V1.4 <- cartera.4.proyectada %>%
  group_by(Trayectoria) %>%
  summarise(V1 = sum(delta * S1)) %>%
  pull(V1)

#Métricas del resultado:
valor.esperado.4 <- mean(V1.4, na.rm = T)

sd(V1.4)

quantile(V1.4, probs = c(0.05, 0.5, 0.95))

prob.perdida.4 <- length(V1.4[V1.4 < 102000])/length(V1.4)

SE.pp.4 <- sqrt(prob.perdida.4*(1-prob.perdida.4)/length(V1.4))

SE.Ve.4 <- sd(V1.4)/sqrt(length(V1.4))

IC.media.4 <- c(
  valor.esperado.4 - 1.96*SE.Ve.4,
  valor.esperado.4 + 1.96*SE.Ve.4
)

IC.prob.4 <-  c(
  prob.perdida.4 - 1.96*SE.pp.4,
  prob.perdida.4 + 1.96*SE.pp.4
)


#Cartera 5 (diversificada):
cartera.5.proyectada <- cartera.5 %>% 
  left_join(
    proyecciones.final %>% select(c("Ticker", "Trayectoria", "Mes.12")), 
    by = "Ticker"
  ) %>% 
  select(Ticker, Trayectoria, delta, S1 = Mes.12)

cartera.5.proyectada %>% count(Ticker)

V1.5 <- cartera.5.proyectada %>%
  group_by(Trayectoria) %>%
  summarise(V1 = sum(delta * S1)) %>%
  pull(V1)

#Métricas del resultado:
valor.esperado.5 <- mean(V1.5, na.rm = T)

sd(V1.5)

quantile(V1.5, probs = c(0.05, 0.5, 0.95))

prob.perdida.5 <- length(V1.5[V1.5 < 102000])/length(V1.5)
SE.pp.5 <- sqrt(prob.perdida.5*(1-prob.perdida.5)/length(V1.5))
SE.Ve.5 <- sd(V1.5)/sqrt(length(V1.5))

IC.media.5 <- c(
  valor.esperado.5 - 1.96*SE.Ve.5,
  valor.esperado.5 + 1.96*SE.Ve.5
)

IC.prob.5 <-  c(
  prob.perdida.5 - 1.96*SE.pp.5,
  prob.perdida.5 + 1.96*SE.pp.5
)



c(valor.esperado.1, valor.esperado.2, valor.esperado.3, valor.esperado.4, valor.esperado.5)

#Basura/extra: ----


parametros
#Librería ver si la normal encaja:
#   library(tseries)
# 
# jarque.bera.test(r)





