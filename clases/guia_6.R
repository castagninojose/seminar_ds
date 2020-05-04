# CLASE 20200429

completar_album <- function(n_fig, p_size=1) {
  #' Simulacion de llenar un album de `n_fig` figuritas.
  figuritas <- rep(0, n_fig)
  album_lleno <- FALSE
  rv <- 0
  while (album_lleno == FALSE) {
    rv <- rv + 1
    for (f in sample(1:n_fig, p_size)) {
      figuritas[f] <- TRUE
    }
    album_lleno <- sum(figuritas) == n_fig
  }
  return(rv)
}

repeticiones <- function(n, n_fig=6) {
  rv <- rep(0, n)
  for (r in 1:n){
    rv[r] <- completar_album(n_fig)
  }
  return(rv)
}
mean(repeticiones(1000))
sum(repeticiones(200) <= 16) / 1000
sort(repeticiones(1000))[900]


n_rep <- c(200, 500, 1000, 5000, 10000)
promedios <- rep(NA, 5)
probas <- rep(NA, 5)
cuantiles <- rep(NA, 5)
for (i in 1:length(n_rep)) {
  print(n_rep[i])
  promedios[i] <- mean(repeticiones(n_rep[i]))
  probas[i] <- sum(repeticiones(n_rep[i]) <= 16) / n_rep[i]
  cuantiles[i] <- sort(repeticiones(n_rep[i]))[0.9*n_rep[i]]
}

promedios
probas
cuantiles

repeticiones(100, n_fig=640)

repeticiones()