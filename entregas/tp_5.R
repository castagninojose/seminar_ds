library('readr')
library('docstring')
library('ggplot2')


setwd("/home/puff/git-repos/seminar_ds/data/")
df_hongos <- read_table2("hongos_clasificados.txt")

hist(df_hongos$Height)

gauss_k <- function(x) {
  k <- sqrt(2*pi)*exp(-(x**2)/2)
  return(k)
}

f_sombrero = function(x, k, datos, h) {
  #' Estimacion no paramétrica.
  #' 
  #' @description Estimación no paramétrica de una densidad desconocida
  #' a partir de las observaciones de la muestra.
  #'
  #' @param x float. Punto donde estimar la densidad.
  #' @param k function. El núcleo a usar.
  #' @param datos vector. la muestra.
  #' @param h float. El tamaño de la ventana.
  s <- 0
  n <- length(datos)
  for (i in 1:n) {
    c <- k((x-datos[i]) / h)
    s <- s + c
  }
  rv <- s / (n*h)
  return(rv)
}

class.nopar <- function(x_nuevo, X_obs, Y_obs, h1=0.1, h0=0.33) {
  #' Predecir la variedad de hongo.
  #' 
  #' @description Para predecir el género usa
  #' la regla de Bayes óptima con estimaciones
  #' no paramétricas de las densidades a posteriori.
  #'
  #' @param x_nuevo float. Altura del especímen a clasificar.
  #' @param X_obs vector. Las alturas de los hongos de nuestra muestra.
  #' @param Y_obs vector. La variedad de hongo.
  #' @param h0 float. Ventana para usar en la estimacion de f0.
  #' @param h1 float. Ventana para usar en la estimacion de f1.
  m <- Y_obs == 1  # las observaciones de la variedad 1
  f_0 <- f_sombrero(x_nuevo, gauss_k, X_obs[!m], h0)
  f_1 <- f_sombrero(x_nuevo, gauss_k, X_obs[m], h1)
  
  p <- f_1 * (1 - mean(m)) >= (f_0 * mean(m))
  
  return(ifelse(p, 1, 2))
}

# Script para jugar con distintas ventanas inspirado en lo que vimos en clase.
h0 <- seq(0.1, 1, 0.1)
h1 <- seq(0.1, 1, 0.1)
clasificados <- c()
results <- matrix(data=NA, nrow=length(h0), ncol=length(h1))
for (i in 1:length(h0)) {
  for (j in 1:length(h1)) {
    for (k in 1:length(df_hongos$Variety)) {
      shit_df <- df_hongos[-k,]
      new_r = class.nopar(
        x_nuevo=df_hongos$Height[k],
        X_obs=shit_df$Height,
        Y_obs=shit_df$Variety,
        h0=h0[i],
        h1=h1[j]
      )
      clasificados[k] <- new_r
    }
    results[i,j] <- mean(clasificados != df_hongos$Variety)
  }
}

results[which.min(results)]
alcanza_minimo_en <- which(results==min(results), arr.ind=TRUE)
h0[4]
h1[10]

for (k in 1:length(df_hongos$Variety)) {
  shit_df <- df_hongos[-k,]
  new_r = class.nopar(
    x_nuevo=df_hongos$Height[k],
    X_obs=shit_df$Height,
    Y_obs=shit_df$Variety,
    h0=0.33,
    h1=0.1
  )
  clasificados[k] <- new_r
}
mean(clasificados != df_hongos$Variety)















alturas <- seq(0, 13, 0.1)
est_f <- lapply(
  alturas,
  f_sombrero,
  k=gauss_k,
  datos=df_hongos$Height,
  h=2
)

plot_df <- data.frame(
  x=alturas,
  f_s=as.numeric(est_f)
)

g <- ggplot(data=plot_df, aes(alturas))
g <- g + geom_line(aes(x=alturas, y=est_f, color="h=0.1"))
#g <- g + geom_line(aes(x=, y=h_0.1, color="h=0.1"))
#g <- g + geom_line(aes(x=centros, y=h_1, color="h=1"))
#g <- g + geom_line(aes(x=centros, y=h_5, color="h=5"))
#g <- g + labs(title="Predicciones", x="Alturas madre", y="Alturas hijo")
g

