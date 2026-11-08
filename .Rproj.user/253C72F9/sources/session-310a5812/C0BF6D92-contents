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
