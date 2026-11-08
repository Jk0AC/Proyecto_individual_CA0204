


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


