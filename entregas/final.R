library('readr')
library('docstring')
library('ggplot2')


setwd("/home/puff/git-repos/seminar_ds/entregas/")

install.packages("plotly")
library('plotly')


tcl_viz <- function(dist=rbinom, trials=12, sample_size=19) {
  sabe <- seq(0, sample_size, 100)
  normal <- lapply(sabe, dnorm, mean=sample_size*0.5, sd=sample_size*0.25)
  fig <- plot_ly(x = ~rbinom(trials, sample_size, 0.5), type = "histogram")
  fig <- fig %>% add_trace(type='line', x=sabe, y=normal)
}
tcl_viz()
