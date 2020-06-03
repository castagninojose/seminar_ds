# Trabajo Práctico N°4
## Clasificación

En este trabajo vamos a armar tres clasificadores. A partir de la altura
de una persona le asignarán `F` o `M`, cada uno usando los datos a su manera para hacer la predicción;

* `clasifico_vecinos()` usando la regla de la mayoría de los KNN.
* `clasifico_movil()` usando regla de la mayoría en una ventana al rededor de la observación.
* `clasifico_generativo()` usando la regla óptima de Bayes.

Para los dos primeros casos, me resultó cómodo armarme una función que calcule la moda de un factor
(en nuestro caso de dos niveles, `F` y `M`).

```{r}
moda <- function(v) {
  #' @descrption Función genérica para calcular la moda
  #' de un vector `v` cualquiera.
  #'
  #' @param v vector. En nuestro caso sera un factor de niveles {F, M}.
  t <- table(v)
  return(names(t)[which.max(t)])
}
```

Con esta función puedo armar los dos primeros clasificadores.

### K vecinos más cercanos

En el caso de KNN usé
la función `order()` para conseguir los vecinos más cercanos. Esta operación para nuestro
caso funciona muy bien pero podría tener problemas para escalar a medida que crece el volumen de datos.

```{r}
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
```

Podemos ver que funciona de manera "esperada" para los casos de `x_nuevo=165` y `x_nuevo=175` cm.

> clasifico_vecinos(df_estaturas$altura, df_estaturas$genero, 165)
[1] "F"
> clasifico_vecinos(df_estaturas$altura, df_estaturas$genero, 175)
[1] "M"

## Regla de la mayoría con ventana móvil.

```{r}
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
```

Para este caso también vemos que se comporta de manera razonable en los casos simples;

> clasifico_movil(df_estaturas$altura, df_estaturas$genero, 165)
[1] "F"
> clasifico_movil(df_estaturas$altura, df_estaturas$genero, 175)  
[1] "M"

## Regla óptima de Bayes

Para este caso armé un indicador de las personas de género `F`. Esto es; un vector de que tiene
`TRUE` en los indices en los que nuestra variable `Y_obs` tiene `F`. Con ésto puedo hacer muchas de las cuentas que necesito. Ya sea en la estimación de $$P(Y=1)$$ o de las densidades $$f_0$$ y $$f_1$$. Por último, con todo esto
armo el predicado de la condición $$g_op = 1$$ que uso para convertir en `F` o `M` según corresponda.

```{r}
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
```

Como los anteriores, se comporta razonablemente para los casos típicos.

> clasifico_generativo(df_estaturas$altura, df_estaturas$genero, 165)
[1] "F"
clasifico_generativo(df_estaturas$altura, df_estaturas$genero, 175)
[1] "M"

# 10 Errores empíricos.

Con las funciones, uso `lapply()` para generar las predicciones de los datos de testeo.

```{r}
yhats_knn <- lapply(
  df_testeo$altura,
  clasifico_vecinos,
  Y_obs=df_estaturas$genero,
  X_obs=df_estaturas$altura
)

yhats_movil <- lapply(
  df_testeo$altura,
  clasifico_movil,
  Y_obs=df_estaturas$genero,
  X_obs=df_estaturas$altura
)

yhats_gen <- lapply(
  df_testeo$altura,
  clasifico_generativo,
  Y_obs=df_estaturas$genero,
  X_obs=df_estaturas$altura
)
```

> mean(yhats_gen != df_testeo$genero)
[1] 0.03225806
mean(yhats_knn != df_testeo$genero)
[1] 0.03225806
mean(yhats_movil != df_testeo$genero)
[1] 0.06451613
