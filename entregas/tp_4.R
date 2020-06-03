library(readr)
library(docstring)
library(ggplot2)
setwd('/home/jose/git-repos/seminar_ds')

moda <- function(v) {
  #' @descrption Función genérica para calcular la moda
  #' de un vector `v` cualquiera.
  #'
  #' @param v vector. En nuestro caso sera un factor de niveles {F, M}.
  t <- table(v)
  return(names(t)[which.max(t)])
}

# 1.1
df_estaturas <- read_csv("alturas_n_500.csv")
df_testeo <- read_csv("alturas_testeo.csv")
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
  #' @description Usa la regla de la mayoría para los `k` vecinos más 
  #' cercanos (KNN) para clasificar a los individuos en "F" o "M" según
  #' su altura.
  #' 
  #' @param X_obs vector. Las alturas de las personas.
  #' @param Y_obs factor. El género de las personas.
  #' @param x_nuevo float. Altura de la persona a clasificar.
  #' @param k int. Cantidad de vecinos más cercanos.
  dist <- abs(x_nuevo - X_obs)
  mas_cercanos <- Y_obs[order(dist)][1:k]
  return(moda(mas_cercanos))
}

clasifico_vecinos(df_estaturas$altura, df_estaturas$genero, 165)
clasifico_vecinos(df_estaturas$altura, df_estaturas$genero, 175)

# 2.5
clasifico_movil <- function(X_obs, Y_obs, x_nuevo, h=1) {
  #' @description  Predecir género con regla de la mayoria con ventana móvil.
  #' 
  #' @param X_obs vector. Las alturas de las personas.
  #' @param Y_obs factor. El género de los personas.
  #' @param x_nuevo float. Altura de la persona a clasificar.
  #' @param h float. Radio de la ventana movil.
  vent <- abs(x_nuevo - X_obs) <= h
  return(moda((Y_obs[vent])))
}

clasifico_movil(df_estaturas$altura, df_estaturas$genero, 165)
clasifico_movil(df_estaturas$altura, df_estaturas$genero, 175)  


# 3.9
clasifico_generativo <- function(X_obs, Y_obs, x_nuevo) {
  #' Predecir género con método generativo.
  #' 
  #' @description Para predecir el género usa
  #' la regla de Bayes óptima estimando a partir de nuestros
  #' datos las densidades a posteriori y la P(Y=1).
  #'  
  #' @param X_obs vector. Las alturas de los individuos.
  #' @param Y_obs vector. El género de los individuos.
  #' @param x_nuevo float. Altura del individuo a clasificar.
  m <- Y_obs == 'F'
  f_0 <- dnorm(x_nuevo, mean=mean(X_obs[!m]), sd=sd(X_obs[!m]))
  f_1 <- dnorm(x_nuevo, mean=mean(X_obs[m]), sd=sd(X_obs[m]))
  
  p <- f_1 * (1 - mean(m)) >= (f_0 * mean(m))

  return(ifelse(p, 'F', 'M'))
}

clasifico_generativo(df_estaturas$altura, df_estaturas$genero, 165)
clasifico_generativo(df_estaturas$altura, df_estaturas$genero, 175)

# 10 Errores empíricos
yhats_test_knn <- lapply(
  df_testeo$altura,
  clasifico_vecinos,
  Y_obs=df_estaturas$genero,
  X_obs=df_estaturas$altura
)

yhats_test_movil <- lapply(
  df_testeo$altura,
  clasifico_movil,
  Y_obs=df_estaturas$genero,
  X_obs=df_estaturas$altura
)

yhats_test_gen <- lapply(
  df_testeo$altura,
  clasifico_generativo,
  Y_obs=df_estaturas$genero,
  X_obs=df_estaturas$altura
)

mean(yhats_test_gen != df_testeo$genero)
mean(yhats_test_knn != df_testeo$genero)
mean(yhats_test_movil != df_testeo$genero)

# 5.11

x_nuevos <- seq(160, 170, .01)

yhats_knn <- lapply(
  x_nuevos,
  clasifico_vecinos,
  Y_obs=df_estaturas$genero,
  X_obs=df_estaturas$altura
)

yhats_movil <- lapply(
  x_nuevos,
  clasifico_movil,
  Y_obs=df_estaturas$genero,
  X_obs=df_estaturas$altura
)

yhats_gen <- lapply(
  x_nuevos,
  clasifico_generativo,
  Y_obs=df_estaturas$genero,
  X_obs=df_estaturas$altura
)

# yha11 <- lapply(
#   x_nuevos,
#   clasifico_vecinos,
#   Y_obs=df_estaturas$genero,
#   X_obs=df_estaturas$altura,
#   k=10
# )

df_clasificadores <- data.frame(
  x=x_nuevos,
  knn=yhats_knn,
  mov=yhats_movil,
  gen=yhats_gen
)

g <- ggplot(df_clasificadores, aes(x_nuevos))
g <- g + geom_line(aes(x=x_nuevos, y=yhats, color="h=0.1"))
# g <- g + geom_line(aes(x=centros, y=h_1, color="h=1"))
# g <- g + geom_line(aes(x=centros, y=h_5, color="h=5"))
# g <- g + labs(title="Predicciones", x="Alturas madre", y="Alturas hijo")
g
plot(df_clasificadores$x, df_clasificadores$knn)
