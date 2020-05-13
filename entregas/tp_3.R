library(docstring)
library(ggplot2)


setwd("/home/puff/git-repos/seminar_ds/entregas/")
df_estaturas <- read.csv("alturas_n_500.csv")

varones_mask <- df_estaturas$genero == "M"
mujeres_mask <- !varones_mask
madre_bajita_m <- df_estaturas$genero == "M" & df_estaturas$contextura_madre == "bajita"


generar_ventana <- function(df, x=156, h=1) {
  df$altura_madre <= x + h & df$altura_madre >= x - h
}


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
  

#2
colnames(df_estaturas)

#3
mean(df_estaturas$altura)

#4
hist(df_estaturas$altura, breaks=11)

#5
plot(density(df_estaturas$altura))

#6
g <- ggplot(data=df_estaturas, aes(altura, color=genero))
g + geom_histogram(aes(x=altura, color=genero))


#7
#plot(df_estaturas$altura_madre, df_estaturas$altura)
g <- ggplot(data=df_estaturas, aes(altura))
g <- g + geom_histogram(aes(x=altura, y=..density..), binwidth=1, fill='blue')
g <- g + geom_density() + facet_grid(.~genero)
g


#7
#plot(df_estaturas$altura_madre, df_estaturas$altura)
g <- ggplot(data=df_estaturas, aes(altura, color=genero))
g <- g + geom_histogram(aes(x=altura, color=genero))
g <- g + geom_density()
g

#10
#plot(df_estaturas$altura_madre, df_estaturas$altura)
g <- ggplot(data=df_estaturas, aes(altura, altura_madre, color=genero))
g + geom_point()

#11
df_varones = df_estaturas[varones_mask,]
sum(df_varones$altura_madre == 156)

#12.a
sum(generar_ventana(df_varones, h=0.5))

#12.b
mean(df_estaturas$altura[generar_ventana(df_varones)])

#13
sum(generar_ventana(df_varones, h=1))
mean(df_estaturas$altura[generar_ventana(varones, h=1)])

#14
sum(generar_ventana(df_varones, x=159, h=1))
mean(df_estaturas$altura[generar_ventana(varones, h=1)])

mean(df_estaturas$altura[generar_ventana(varones, x=157, h=3)])
mean(df_estaturas$altura[generar_ventana(varones, x=160, h=3)])

g <- ggplot(data=df_varones, aes(altura))
g + geom_point()

#15


#16
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

g <- ggplot(df_pred, aes(centros))
g <- g + geom_line(aes(x=centros, y=h_0.1, color="h=0.1"))
g <- g + geom_line(aes(x=centros, y=h_1, color="h=1"))
g <- g + geom_line(aes(x=centros, y=h_5, color="h=5"))
g <- g + labs(title="Predicciones", x="Alturas madre", y="Alturas hijo")
g
