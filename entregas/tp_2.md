# Trabajo Práctico n° 2

Voy a empezar armando una función para simular un experimento genérico. Dicho experimento
tiene dos posibles resultados, que llamaremos "éxito" y "fracaso". Esta función tomará
un argumento `p` que representa la probabilidad de éxito del experimento.

```
exito_fracaso <- function(p) {
  #' Funcion predicado que usa `runif()` para simular un experimento.
  rv <- ifelse(runif(1) < p, 1, 0)
  return(rv)
}
```

Ahora usando la función anterior, voy a armar otra que repita el experimento `n` veces
y me devuelva los resultados. Devuelve un vector de longitud `n` con `0`s y `1`s.
```
repito_exito_fracaso <- function(n, p) {
  #' Realizar `n` veces el experimento `exito_fracaso` y guardar los resultados de cada uno.
  repeticiones <- rep(NA, n)
  for (r in repeticiones) {
    r <- exito_fracaso(p)
  }
  return(repeticiones)
}
```

Como los fracasos los representa con un `0` y los éxitos con un `1`,  
para saber cuantos éxitos hubo en las repeticiones, puedo sumar las componentes del resultado.
```
cuantos_exitos <- function(n, p) {
  #' Calcula la cantidad de exitos del experimento en `n` repeticiones.
  return(sum(repito_exito_fracaso(n, p)))
}
```

Por último, voy a usar la siguiente función para contar cuantas veces hay que
repetir el experimento hasta obtener al primer éxito.
```
perseverancia_exito <- function(p) {
  #' Cuenta la cantidad de experimentos necesarios hasta obtener el primer éxito.
  repeticiones <- 0
  exito <- 0
  while (exito == 0) {
    repeticiones <- repeticiones + 1
    exito <- exito_fracaso(p)
  }
  
  return(repeticiones)
}
```

```
muchas_perseverancia_exito <- rep(NA, 1000)
for (i in 1:1000) {
  muchas_perseverancia_exito[i] <- perseverancia_exito(0.5)
}
table(muchas_perseverancia_exito) / 1000
mean(muchas_perseverancia_exito)
```

```
grid <- seq(0.01, 0.99, by=0.02)
yy <- lapply(grid, perseverancia_exito)
plot(grid, yy, type="l", main='Intentos hasta el primer éxito')
```

