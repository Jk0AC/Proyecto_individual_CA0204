
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

