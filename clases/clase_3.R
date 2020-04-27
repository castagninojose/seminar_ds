library(ggplot2)

#1.1
funcion_partida <- function(x, a=10, b=15) {
  if (x < a){ 
    rv <- 0
  }
  else if (x > b) {
    rv <- 1
  }
  else {
    rv <- (x-10)/(b-a)
  }
  return(rv)
}
#1.2
grid <- seq(5, 20, by=0.01)
yy <- lapply(grid, funcion_partida)
plot(grid, yy, type="l", main='Una funcion partida')

#2.1 y 2.2

suma_positivos <- function(v) {
  rv <- 0
  for (i in v) {
    if (i > 0) {
      rv <- rv + i
    }
  }
  return(rv)
}

suma_positivos(seq(864, 4640))

#2.3

suma_si_hay_positivos <- function(v) {
  rv <- 0
  for (i in v) {
    if (i > 0) {
      rv <- rv + i
    }
  }
  if (rv > 0) {
    return(rv)  
  }
  else {
    return("No hay positivos.")
  }
}


#3.1

suma_cubilete <- function(n=5) {
  dado <- c(1, 2, 3, 4, 5, 6)
  caras_dado <- sample(dado, n, replace=TRUE)

  return(sum(caras_dado))
}

#3.3

dados_compas <- read.csv('resultados_dados.csv')
hist(dados_compas$Valor.de.la.suma.obtenido, main='Resultados de mis compañeros')

#3.4 y 3.5

histograma_dados <- function(repeticiones=40) {
  r <- rep(0, repeticiones)
  
  for (k in 1:length(r)) {
    r[k] <- suma_cubilete()
  }

  hist(r, breaks=24, col='darkmagenta', main='Resultados de la suma')
}


#3.6

todos_seis <- function() {
  lanzamientos <- 0
  generala_de_6 <- FALSE

  while (generala_de_6 == FALSE) {
    lanzamientos <- lanzamientos + 1
    generala_de_6 <- suma_cubilete() == 30
  }

  return(lanzamientos)
}
