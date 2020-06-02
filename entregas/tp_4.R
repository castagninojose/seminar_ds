library(readr)
library(ggplot2)
# setwd('/home/jose/git-repos/seminar_ds')

moda <- function(v) {
  #' Moda.
  #'
  #' @descrption Función genérica para calcular la moda
  #' de un vector `v` cualquiera (numérico o no).
  #'
  #' @param v vector. En nuestro caso sera un factor de dos niveles (F y M).
  
  t <- table(v)
  return(names(t)[which.max(t)])
}

# 1.1
df_estaturas <- read.csv("~/Documents/cms/seminar/data/alturas_n_500.csv")
g <- ggplot(data=df_estaturas, aes(altura))
g <- g + geom_histogram(aes(x=altura, y=..density..), binwidth=1, fill='blue')
g <- g + geom_density() + facet_grid(.~genero)
g

# 1.2
dist <- abs(165 - df_estaturas$altura)
aver <- df_estaturas[ order(dist) , ]
mode(aver$genero[1:12])
as.numeric(aver$genero[1:9]) -1

# 1.3
vent <- df_estaturas$altura<166.5 & df_estaturas$altura>163.5
df_estaturas[vent,]

# 2.4
clasifico_vecinos <- function(X_obs, Y_obs, x_nuevo, k=10) {
  #' Predecir género con KNN.
  #' 
  #' @description Usa la regla de la mayoría para los `k` vecinos más cercanos (KNN)
  #' para clasificar a los individuos en "F" o "M" según su altura.
  #' 
  #' @param X_obs vector. Las alturas de las personas.
  #' @param Y_obs vector. El género de los individuos.
  #' @param x_nuevo float. Altura del individuo a clasificar.
  #' @param k int. Cantidad de vecinos más cercanos.
  dist <- abs(x_nuevo - X_obs)
  mas_cercanos <- Y_obs[order(dist)][1:k]
  return(moda(mas_cercanos))
}

clasifico_vecinos(df_estaturas$altura, df_estaturas$genero, 165)
clasifico_vecinos(df_estaturas$altura, df_estaturas$genero, 175)

# 2.5
clasifico_movil <- function(X_obs, Y_obs, x_nuevo, h=1) {
  #' Predecir género con promedios móviles.
  #' 
  #' @description Para la prediccion de un nuevo `X` usa
  #' un promedio movil centrado en `x_nuevo` de tamaño `h`
  #' y una nueva observación de `Y`.
  #' 
  #' @param X_obs vector. Las alturas de las madres.
  #' @param Y_obs vector. Las alturas de los individuos.
  #' @param x_nuevo float. Altura del individuo a clasificar.
  #' @param h float. Radio de la ventana movil.
  vent <- abs(x_nuevo - X_obs) <= h
  return(moda((Y_obs[vent])))
}

clasifico_movil(df_estaturas$altura, df_estaturas$genero, 165)
clasifico_movil(df_estaturas$altura, df_estaturas$genero, 175)  


# 3.9
clasifico_generativo <- function(X_obs, Y_obs, x_nuevo) {
  proba_0 <- mean(Y_obs=='M')
  mu_0 <- mean(X_obs[Y_obs=='M'])
  mu_1 <- mean(X_obs[Y_obs=='F'])
  sigma_0 <- sd(X_obs[Y_obs=='M'])
  sigma_1 <- sd(X_obs[Y_obs=='F'])
  f_0 <- dnorm(x_nuevo, mean=mu_0, sd=sigma_0)
  f_1 <- dnorm(x_nuevo, mean=mu_1, sd=sigma_1)
  
  p <- ( f_1 * (1 - proba_0) ) >= (f_0 * proba_0)

  return(sum(p))
}

clasifico_generativo(df_estaturas$altura, df_estaturas$genero, 165)
clasifico_generativo(df_estaturas$altura, df_estaturas$genero, 175)
