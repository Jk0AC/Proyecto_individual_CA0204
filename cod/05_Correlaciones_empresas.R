
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

