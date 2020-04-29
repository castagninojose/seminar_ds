# Trabajo Práctico n° 2

Voy a empezar armando una función para simular un experimento genérico. Dicho experimento
tiene dos posibles resultados, que llamaremos "éxito" y "fracaso". Esta función tomará
un argumento `p` que representa la probabilidad de éxito del experimento. En el caso de que
quisiéramos emular un experimento con una probabilidad de éxito fija, como por ejemplo `0.5` 
si se tratara del lanzamiento de una moneda equilibrada, este argumento podríamos obviarlo.

```
exito_fracaso <- function(p) {
  #' Funcion predicado que usa `runif()` para simular un experimento.
  rv <- ifelse(runif(1) < p, 1, 0)
  return(rv)
}
```

Ahora usando la función anterior, voy a armar otra que repita el experimento `n` veces
y me devuelva los resultados obtenidos. Devuelve un vector de longitud `n` con ceros y unos.

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
para saber cuantos éxitos hubo en total en todas las las repeticiones, 
puedo sumar las componentes del resultado.
```
cuantos_exitos <- function(n, p) {
  #' Calcula la cantidad de exitos del experimento en `n` repeticiones.
  return(sum(repito_exito_fracaso(n, p)))
}
```

### a)
Voy a usar `exito_fracaso()` para construir una función que cuenta cuantas veces hay que
repetir el experimento hasta obtener el primer éxito.

Al igual que en el caso de `exito_fracaso()` esta función toma un argumento `p` que para este 
primer ítem no es necesario. Notar que el llamado lo hice para `p = 1/2` (i.e.) una moneda justa.

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

Con todas estas funciones a disposición, puedo contestar las preguntas de la consigna.  
Empiezo simulando 1000 repeticiones del experimento y para saber las frecuencias de los  
resultados uso la funcion `table()`.  

### b)

```
muchas_perseverancia_exito <- rep(NA, 1000)
for (i in 1:1000) {
  muchas_perseverancia_exito[i] <- perseverancia_exito(0.5)
}
table(muchas_perseverancia_exito) / 1000
mean(muchas_perseverancia_exito)
```

> 0.499 0.255 0.128 0.057 0.027 0.017 0.007 0.005 0.004 0.001 
> [1] 1.993

Analizando estos resultados podemos conjeturar que se comporta exponencialmente. Esto es,  
la cantidad de intentos necesarios para alcanzar el primer éxito disminuye exponencialmente
a medida que crece la probabilidad de éxito `p`.

### c)

Voy a reutilizar lo anterior para armar una funcion que me facilite plotear para cada `p`.

```
muchas_perseverancia_exito <- function(p) {
  rv <- rep(NA, 1000)
  for (i in 1:1000) {
    rv[i] <- perseverancia_exito(p)
  }
  return(mean(rv))
}
```

Para modelar este comportamiento voy a usar la curva $$y = \frac{1}{x}$$ que entre los valores de 0 y 1
se ajusta a los resultados obtenidos.  
Spoiler: se trata de la ley de los grandes números en acción, y la función ajusta bien porque el  
valor esperado de una distribución geométrica de parámetro $$p$$ es justamente $$\frac{1}{p}$$.

```
modelo <- function(x) {1/x}

p <- seq(0.01, 0.99, by=0.01)
yy <- lapply(p, perseverancia_exito)
zz <- lapply(p  , modelo)
frame <- data.frame(x=p, intentos=as.numeric(yy), exp=as.numeric(zz))

g <- ggplot(frame, aes(p))
g <- g + geom_line(aes(x=p, y=intentos, colour="blue"), size=2)
g <- g + geom_line(aes(x=p, y=exp, colour="yellow"), size=0.666)
g <- g + scale_color_identity(guide="legend", name="", labels = c("Resultados", "Modelo"))
g
```

Corriendo lo anterior genero el siguiente gráfico:  
![alt text](https://drive.google.com/uc?id=1-obV0YfEbZEMCqsISXpfXSEwGufYAsQw)
