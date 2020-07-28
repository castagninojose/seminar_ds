# T.P. N°5.

Además de la función `class.nopar` para clasificar los hongos incluyo algunas de las
cuales esta depende para que puedan probarla correctamente. Espero haber respetado
las notaciones de los inputs que mencionaron en clase.

Además puse un pequeño script que usé para buscar las `h0` y `h1` usando los errores
del clasificador.


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

```{r}
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
```

#### "grid search"

```
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
```
Una vez corrido ese script, podemos buscar los minimos con `results[which.min(results)]`
o con `which(results==min(results), arr.ind=TRUE)` e ir refinando los `h`.