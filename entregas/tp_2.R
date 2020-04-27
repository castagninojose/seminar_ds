exito_fracaso <- function(p) {
  rv <- ifelse(runif(1) < p, 1, 0)
  return(rv)
}

repito_exito_fracaso <- function(n, p) {
  repeticiones <- rep(NA, n)
  for (r in repeticiones) {
    r <- exito_fracaso(p)
  }
  return(repeticiones)
}

cuantos_exitos <- function(n, p) {
  return(sum(repito_exito_fracaso(n, p)))
}


perseverancia_exito <- function(p) {
  repeticiones <- 0
  exito <- 0
  while (exito == 0) {
    repeticiones <- repeticiones + 1
    exito <- exito_fracaso(p)
  }
  
  return(repeticiones)
}

muchas_perseverancia_exito <- rep(NA, 1000)
for (i in 1:1000) {
  muchas_perseverancia_exito[i] <- perseverancia_exito(0.5)
}
table(muchas_perseverancia_exito) / 1000
mean(muchas_perseverancia_exito)


grid <- seq(0.01, 0.99, by=0.02)
yy <- lapply(grid, perseverancia_exito)
plot(grid, yy, type="l", main='Intentos hasta el primer éxito')

