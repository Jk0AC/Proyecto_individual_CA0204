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

#Función que recibe el último precio de la acción, su retorno esperado, la volatilidad
#histórica, salto de tiempo y un valor particular de Z. Devuelve el precio
#proyectado del GBM en el tiempo discreto t
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

##(iv)----

#Función que recibe un vector (vector con valores de proyecciones de la cartera)
#y devuelve un tibble con un resumen estadístico: esperanza, VaR, 
#probabilidad de pérdida y errores estándar variables simuladas: valor esperado, 
#probabilidad de pérdida

resumen.cartera <- function(V1, Vo = 102000){
  
  # Valor esperado
  valor.esperado <- mean(V1)
  
  # VaR empírico (percentil 5%)
  VaR <- quantile(V1, probs = 0.05)
  
  # Mediana (percentil 50%)
  mediana <- quantile(V1, probs = 0.50)
  
  # Probabilidad de pérdida
  prob.perdida <- mean(V1 < Vo)
  
  # Error estándar del valor esperado
  EE.esperanza <- sd(V1) / sqrt(length(V1))
  
  # Error estándar de la probabilidad de pérdida
  EE.prob.perdida <- sqrt(
    prob.perdida * (1 - prob.perdida) /
      length(V1)
  )
  
  tibble::tibble(
    `Valor esperado` = valor.esperado,
    `VaR (5%)` = as.numeric(VaR),
    `Mediana (50%)` = as.numeric(mediana),
    `Probabilidad pérdida` = prob.perdida,
    `EE esperanza` = EE.esperanza,
    `EE prob pérdida` = EE.prob.perdida
  )
  
}
