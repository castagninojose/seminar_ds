# TP n°1

library("PASWR2")
titanic <- TITANIC3


clases <- unique(titanic$pclass)

### a)
for (cl in clases) {
  print(paste(cl, mean(titanic[titanic$pclass == cl,]$survived)))
}

### b)
for (cl in clases) {
  
  v_mask <- titanic$pclass == cl & titanic$sex == 'male'
  m_mask <- titanic$pclass == cl & titanic$sex == 'female'
  
  print(paste('Mujeres,', 'clase:', cl, mean(titanic[m_mask, ]$survived)))
  print(paste('Varones,', 'clase:', cl, mean(titanic[v_mask, ]$survived)))
}

### c)
m_s <- titanic$sex == 'female' & titanic$survived == 1
print(paste('La más longeva:', max(titanic[m_s, ]$age, na.rm=TRUE)))
