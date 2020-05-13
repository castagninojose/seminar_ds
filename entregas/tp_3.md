# Trabajo Práctico N° 3

## Funciones

Mediante el uso de una función vamos a intentar estimar la altura de un individuo a partir de la altura de su madre. Esta función se va a servir de un conjunto de datos de otros individuos para hacer la predicción.

Armando este informe me di cuenta que la solución a los problemas 16 y 18 eran prácticamente idénticas, así que decidí resolver todo usando una misma función, la siguiente: 

### Ejercicios 16 y 18
```{r}
predigo_ventana <- function(X_obs, Y_obs, x_nuevo, h) {
  #' Predecir altura.
  #' 
  #' @description Para la prediccion de un nuevo `X` usa
  #' un promedio movil centrado en `x_nuevo` de tamaño `h`
  #' y una nueva observación de `Y`.
  #' 
  #' @param X_obs vector. Las alturas de las madres.
  #' @param Y_obs vector. Las alturas de los individuos.
  #' @param x_nuevo float. Altura de la madre nueva.
  #' @param h float. Radio de la ventana movil.
  
  m <- (X_obs <= x_nuevo + h) & (X_obs >= x_nuevo - h)
  if (sum(m) == 0) {  # casos con ninguna Y_obs dentro de la ventana.
    rv <- NA
  }
  else {
    rv <- mean(Y_obs[m])
  }
  return(rv)
}
```

Nota: opté por agregarle `_obs` a los nombres de los parámetros porque eso después me molestaba a la hora de usar `lapply` para plotear. Al parecer esta función usa un parámetro propio llamado `X` entonces usar el mismo nombre causaba colisiones.

Con esta función a mano, voy a armar un data frame con los resultados para los distintos centros (alturas de madres nuevas) y los `h`.


```
df_varones = df_estaturas[varones_mask,]
a <- min(df_varones$altura_madre)
b <- max(df_varones$altura_madre)
centros <- seq(a, b, length.out=50)

predicciones_0.1 <- lapply(
  centros,
  predigo_ventana,
  Y_obs=df_varones$altura,
  X_obs=df_varones$altura_madre,
  h=0.5
)

predicciones_1 <- lapply(
  centros,
  predigo_ventana,
  Y_obs=df_varones$altura,
  X_obs=df_varones$altura_madre,
  h=1
)

predicciones_5 <- lapply(
  centros,
  predigo_ventana,
  Y_obs=df_varones$altura,
  X_obs=df_varones$altura_madre,
  h=5
)

df_pred <- data.frame(
  x=centros,
  h_0.1=as.numeric(predicciones_0.1),
  h_1=as.numeric(predicciones_1),
  h_5=as.numeric(predicciones_5)
)
```

Con esto ya estoy listo para armar el plot.

```
g <- ggplot(df_pred, aes(centros))
g <- g + geom_line(aes(x=centros, y=h_0.1, color="h=0.1"))
g <- g + geom_line(aes(x=centros, y=h_1, color="h=1"))
g <- g + geom_line(aes(x=centros, y=h_5, color="h=5"))
g <- g + labs(title="Predicciones", x="Alturas madre", y="Alturas hijo")
g
```
![alt text](https://drive.google.com/uc?id=1KWkCzXr58iSnO_Y4IHzRadxr9lYur_RQ)

Puedo observar como para `h=5` la función empieza a perder poder predictivo. Esto tiene sentido; si lo pensamos en un extremo (`h` muy grande) la ventana esencialmente va a ser como mirar todos los datos, y en ese caso la predicción terminará devolviendo siempre el mismo valor constante (la media).

### Ejercicio 19
Trabajo en una empresa de software (de datos, como programador) así que bueno, podría contar varios ejemplos. Por ejemplo ahora estoy colaborando en una solución para detectar anomalías en el uso de datos para una empresa de telefonía móvil. En este contexto se usan los datos de compras/recargas como regresor para estimar la cantidad de tráfico. Esta estimación se usa luego para detectar posibles outliers.