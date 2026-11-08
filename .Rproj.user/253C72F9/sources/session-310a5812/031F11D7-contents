
# Modelo Lognormal y Monte Carlo: ----

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

