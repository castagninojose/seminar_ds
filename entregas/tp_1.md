# TP n°1


Como primer paso voy a dejar una copia de los datos en `titanic`, corriendo
```{r}
library("PASWR2")
titanic <- TITANIC3
```

### a)

Empecemos notando que las classes están en la columna `pclass`. Podemos obtener sus valores usando la función `unique`, así:
  
  ```{r}
clases <- unique(titanic$pclass)
```

Por otro lado, usando `df[df$col == val, ]` obtengo los datos del dataframe `df` que en la columna _col_ tienen el valor _val_.

Con ésto a mano, voy a pedirle a R los datos filtrando por clase y luego usar la funcion `mean()` para sacar el promedio de supervivencia.
```{r}
for (cl in clases) {
  print(paste(cl, mean(titanic[titanic$pclass == cl,]$survived)))
}
```
>[1] "1st 0.619195046439629"
[1] "2nd 0.429602888086643"
[1] "3rd 0.255289139633286"

### b)
Para esta parte me alcanza con agregar otro filtro esta vez para el sexo. Me pareció que ayudaba a leer el codigo usar unas mascaras para los respectivos filtros. 
```{r}
for (cl in clases) {
  
  v_mask <- titanic$pclass == cl & titanic$sex == 'male'
  m_mask <- titanic$pclass == cl & titanic$sex == 'female'
  
  print(paste('Mujeres,', 'clase:', cl, mean(titanic[m_mask, ]$survived)))
  print(paste('Varones,', 'clase:', cl, mean(titanic[v_mask, ]$survived)))
}
```

>[1] "Mujeres, clase: 1st 0.965277777777778"
[1] "Varones, clase: 1st 0.340782122905028"
[1] "Mujeres, clase: 2nd 0.886792452830189"
[1] "Varones, clase: 2nd 0.146198830409357"
[1] "Mujeres, clase: 3rd 0.490740740740741"
[1] "Varones, clase: 3rd 0.152129817444219"

### c)
Por ultimo, puedo usar esta vez la función `max()` para quedarme con la superviviente más longeva.

```
m_s <- titanic$sex == 'female' & titanic$survived == 1
print(paste('La más longeva:', max(titanic[m_s, ]$age, na.rm=TRUE)))
```

>[1] "La mas longeva: 76"