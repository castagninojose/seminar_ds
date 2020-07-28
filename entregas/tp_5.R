library('readr')
library('docstring')
library('ggplot2')


setwd("/home/puff/git-repos/seminar_ds/data/")
df_hongos <- read_table2("hongos_clasificados.txt")

df_train <- 
df_test <- 

hist(df_hongos$Height)

gauss_k <- function(x) {
  k <- sqrt(2*pi)*exp(-(x**2)/2)
  return(k)
}

f_sombrero = function(x, k, datos, h) {
  s <- 0
  n <- length(datos)
  for (i in 1:n) {
    c <- k((x-datos[i]) / h)
    s <- s + c
  }
  rv <- s / (n*h)
  return(rv)
}

class.nopar <- function(X_new,  X_datos, Y_datos, h1, h0) {

  m <- Y_datos == 1
  f_0 <- f_sombrero(X_new, gauss_k, X_datos[!m], h0)
  f_1 <- f_sombrero(X_new, gauss_k, X_datos[m], h1)
  
  p <- f_1 * (1 - mean(m)) >= (f_0 * mean(m))
  
  return(ifelse(p, 1, 2))
}


results <- c()
for (k in 1:length(df_hongos$Variety)) {
  shit_df <- df_hongos[-k,]
  new_r = class.nopar(
    X_new=df_hongos$Height[k],
    X_datos=shit_df$Height,
    Y_datos=shit_df$Variety,
    h0=0.69,
    h1=0.38
  )
  #print(new_r)
  results <- c(results,new_r)
  print(mean(results != df_hongos$Variety))
}



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

