
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

### Cartera 1 (muy volatil): ----

cartera.1 <- muestra.volt %>% 
  filter(Clasificacion.sigma == "Alta σ") %>% 
  select(Ticker) %>% 
  left_join(parametros, by = "Ticker") %>% 
  select(-c("Retorno.esperado.anual", "Volatilidad.anual")) %>% 
  mutate(delta = inversion.empresa / So) 


Vo.1 <- sum(cartera.1$So * cartera.1$delta)
Total.acciones.1 <- sum(cartera.1$delta)

### Cartera 2 (poco volatil): ----

cartera.2 <- muestra.volt %>% 
  filter(Clasificacion.sigma == "Baja σ") %>% 
  select(Ticker) %>% 
  left_join(parametros, by = "Ticker") %>% 
  select(-c("Retorno.esperado.anual", "Volatilidad.anual")) %>% 
  mutate(delta = inversion.empresa / So) 


Vo.2 <- sum(cartera.2$So * cartera.2$delta)
Total.acciones.2 <- sum(cartera.2$delta)

### Cartera 3 (volatilidad media): ----

cartera.3 <- muestra.volt %>% 
  filter(Clasificacion.sigma == "Media σ") %>% 
  select(Ticker) %>% 
  left_join(parametros, by = "Ticker") %>% 
  select(-c("Retorno.esperado.anual", "Volatilidad.anual")) %>% 
  mutate(delta = inversion.empresa / So) 


Vo.3 <- sum(cartera.3$So * cartera.3$delta)
Total.acciones.3 <- sum(cartera.3$delta)

### Cartera 4 (no diversificada): ----

cartera.4 <- parametros %>% 
  filter(Ticker %in% c("AXP", "JPM", "SCHW", "PH", "URI", "LIN")) %>% 
  select(-c("Retorno.esperado.anual", "Volatilidad.anual")) %>% 
  mutate(delta = inversion.empresa / So)

Vo.4 <- sum(cartera.4$So * cartera.4$delta)
Total.acciones.4 <- sum(cartera.4$delta)

### Cartera 5 (diversificada): ----

cartera.5 <- parametros %>% 
  filter(Ticker %in% c("DUK", "TSLA", "LMT", "WBD", "RMD", "CMG")) %>% 
  select(-c("Retorno.esperado.anual", "Volatilidad.anual")) %>% 
  mutate(delta = inversion.empresa / So)

Vo.5 <- sum(cartera.5$So * cartera.5$delta)
Total.acciones.5 <- sum(cartera.5$delta)


#Verificamos que las carteras tengan el mismo valor:

inversion == c(Vo.1, Vo.2, Vo.3, Vo.4, Vo.5)

##Proyección carteras ----

### Proyección Cartera 1 (muy volatil): ----
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

median(V1.1, na.rm = T)

sd(V1.1)

quantiles.distribucion.1 <- quantile(V1.3, probs = c(0.05, 0.5, 0.95))

IC.media.1 <- c(
  valor.esperado.1 - 1.96*SE.Ve.1,
  valor.esperado.1 + 1.96*SE.Ve.1
)

IC.prob.1 <-  c(
  prob.perdida.1 - 1.96*SE.pp.1,
  prob.perdida.1 + 1.96*SE.pp.1
)

### Proyección Cartera 2 (poco volatil): ----

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

median(V1.2, na.rm = T)

sd(V1.2)

quantiles.distribucion.2 <- quantile(V1.2, probs = c(0.05, 0.5, 0.95))

IC.media.2 <- c(
  valor.esperado.2 - 1.96*SE.Ve.2,
  valor.esperado.2 + 1.96*SE.Ve.2
)

IC.prob.2 <-  c(
  prob.perdida.2 - 1.96*SE.pp.2,
  prob.perdida.2 + 1.96*SE.pp.2
)



### Proyección Cartera 3 (volatilidad media): ----
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
  pull(V1) %>% sort()

#Métricas del resultado:
median(V1.3, na.rm = T)

sd(V1.3)

quantiles.distribucion.3 <- quantile(V1.3, probs = c(0.05, 0.5, 0.95))


IC.media.3 <- c(
  valor.esperado.3 - 1.96*SE.Ve.3,
  valor.esperado.3 + 1.96*SE.Ve.3
)

IC.prob.3 <-  c(
  prob.perdida.3 - 1.96*SE.pp.3,
  prob.perdida.3 + 1.96*SE.pp.3
)


resumen.cartera(V1.3)

### Proyección Cartera 4 (no diversificada): ----
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
  pull(V1) %>% sort()

#Métricas del resultado:


median(V1.4, na.rm = T)

sd(V1.4)


quantiles.distribucion.4 <- quantile(V1.4, probs = c(0.05, 0.5, 0.95))

IC.media.4 <- c(
  valor.esperado.4 - 1.96*SE.Ve.4,
  valor.esperado.4 + 1.96*SE.Ve.4
)

IC.prob.4 <-  c(
  prob.perdida.4 - 1.96*SE.pp.4,
  prob.perdida.4 + 1.96*SE.pp.4
)


### Proyección Cartera 5 (diversificada): ----
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
  pull(V1) %>% sort()

#Métricas del resultado:


median(V1.5, na.rm = T)

sd(V1.5)

quantiles.distribucion.5 <- quantile(V1.5, probs = c(0.05, 0.5, 0.95))

IC.media.5 <- c(
  valor.esperado.5 - 1.96*SE.Ve.5,
  valor.esperado.5 + 1.96*SE.Ve.5
)

IC.prob.5 <-  c(
  prob.perdida.5 - 1.96*SE.pp.5,
  prob.perdida.5 + 1.96*SE.pp.5
)


resumen.carteras <- bind_rows(
  Conservadora = resumen.cartera(V1.2),
  Moderada     = resumen.cartera(V1.3),
  Agresiva     = resumen.cartera(V1.1),
  Concentrada  = resumen.cartera(V1.4),
  Diversificada= resumen.cartera(V1.5),
  .id = "Cartera"
)
