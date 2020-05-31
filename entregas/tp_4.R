library(readr)
library(ggplot2)
# setwd('/home/jose/git-repos/seminar_ds')

moda <- function(v) {
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
  dist <- abs(x_nuevo - X_obs)
  mas_cercanos <- Y_obs[order(dist)][1:k]
  return(moda(mas_cercanos))
}

clasifico_vecinos(df_estaturas$altura, df_estaturas$genero, 165)
clasifico_vecinos(df_estaturas$altura, df_estaturas$genero, 175)

# 2.5
clasifico_movil <- function(X_obs, Y_obs, x_nuevo, h=1) {
  vent <- abs(x_nuevo - X_obs) <= h
  return(moda((Y_obs[vent])))
}

clasifico_movil(df_estaturas$altura, df_estaturas$genero, 165)
clasifico_movil(df_estaturas$altura, df_estaturas$genero, 175)  
