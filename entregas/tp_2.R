library(ggplot2)

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

muchas_perseverancia_exito <- function(p) {
  rv <- rep(NA, 1000)
  for (i in 1:1000) {
    rv[i] <- perseverancia_exito(p)
  }
  return(mean(rv))
}

muchas_perseverancia_exito(0.8)
modelo <- function(x) {
  1/x
}


p <- seq(0.01, 0.99, by=0.01)
yy <- lapply(p, muchas_perseverancia_exito)
zz <- lapply(p, modelo)
frame <- data.frame(x=p, intentos=as.numeric(yy), exp=as.numeric(zz))

g <- ggplot(frame, aes(p))
g <- g + geom_line(aes(x=p, y=intentos, colour="blue"), size=2)
g <- g + geom_line(aes(x=p, y=exp, colour="yellow"), size=0.666)
g <- g + scale_color_identity(guide="legend", name="", labels = c("Resultados", "Modelo"))
g
