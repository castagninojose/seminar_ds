library(ggplot2)

#1
setwd("/home/puff/git-repos/seminar_ds/entregas/")
df_estaturas <- read.csv("alturas_n_500.csv")

varones_mask <- df_estaturas$genero == "M"
mujeres_mask <- !varones_mask
madre_bajita_m <- df_estaturas$genero == "M" & df_estaturas$contextura_madre == "bajita"


generar_ventana <- function(df, x=156, h=1) {
  df$altura_madre <= x + h & df$altura_madre >= x - h
}

preparar_predicciones <- function(df, x=158, h=5) {
  m <- df$altura_madre <= x + h & df$altura_madre >= x - h
  hijos <- df[m,]$altura
  mdes <- df[m,]$altura_madre
  return(c(hijos, mdes))
}

predigo_altura_masculio <- function(altura, altura_madre, altura_mama_nueva=158, h=5) {
  mean(altura)
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

mean(df_estaturas$altura[generar_ventana(varones, c=157, h=3)])
mean(df_estaturas$altura[generar_ventana(varones, c=160, h=3)])

g <- ggplot(data=df_varones, aes(altura))
g + geom_point()

#15


predigo_altura_masculio <- function(
  alturas=df_varones$altura, alturas_mdes=df_varones$altura_madre, x=158, h=5
) {
  mean(alturas[generar_ventana(alturas, x=x, h=h)])
  
}