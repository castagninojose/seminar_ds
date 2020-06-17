setwd("~/git-repos/seminar_ds")
library(readr)
library(ggplot2)

# 1.1
df_gas_1 <- read.csv("data/datos_practica7_gas1.csv")
g <- ggplot(df_gas_1, aes(gas_equipo_1))
g <- g + geom_histogram(bins=30)
g

#1.2
mean(df_gas_1$gas_equipo_1)

# 1.3
quantile(df_gas_1$gas_equipo_1, probs = 0.9)

# 1.4
mean(df_gas_1$gas_equipo_1 - 70 > 2)

# 1.5
mean(df_gas_1$gas_equipo_1[1:5])
mean(df_gas_1$gas_equipo_1[1:30])

quantile(df_gas_1$gas_equipo_1[1:5], probs = 0.9)
quantile(df_gas_1$gas_equipo_1[1:30], probs = 0.9)

# 2.1
df_gas_2 <- read.csv("data/datos_practica7_gas2.csv")
g <- ggplot(df_gas_2, aes(gas_equipo_2))
g <- g + geom_histogram(bins=30)
g

# 2.2
mean(df_gas_2$gas_equipo_2)

# 2.3
quantile(df_gas_2$gas_equipo_2, probs = 0.9)

# 2.4
mean(df_gas_2$gas_equipo_2 - 70 > 2)

# 2.5
mean(df_gas_2$gas_equipo_2[1:5])
mean(df_gas_2$gas_equipo_2[1:30])

quantile(df_gas_2$gas_equipo_2[1:5], probs = 0.9)
quantile(df_gas_2$gas_equipo_2[1:30], probs = 0.9)

