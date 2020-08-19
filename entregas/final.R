library(readr)
library(docstring)
library(ggplot2)
library(plotly)
library(shiny)


setwd("/home/puff/git-repos/seminar_ds/entregas/")



tlc_viz <- function(n=300, sample_size=21) {
  pe <- 0.5
  sim <- rbinom(n, sample_size, pe)
  m <- matrix(sim, n)
  
  sample_means <- rowMeans(m)
  dens <- density(sample_means)
  
  sm_avg <- mean(sample_means)
  sm_sd <- sd(sample_means)
  analytical_sd <- sqrt(n*pe*(1-pe)/sample_size)
  
  
  c)
  return(c(sample_means, dominio, normal_teo))
  
  #fig_ly <- plot_ly(x=sample_means, type = 'histogram', histnorm = 'probability')
  #fig_ly <- fig_ly %>% add_trace(x=dominio, y=normal_teo, type='scatter')
  #fig <- fig %>% add_trace(x=sample_means, y=density, type='scatter')
  
  # fig_gg <- ggplot(data)
  #fig_ly
  return(fig_ly)
}
tlc_viz(n=10, sample_size = 4)


ui <- fluidPage(
  titlePanel("Visualizando TLC"),
  # panel lateral
  sidebarLayout(
    sidebarPanel(
      # input: slider para el tamaño de la muestra
      sliderInput(inputId = "sample_size",
                  label = "Tamaño de las muestras",
                  min = 2,
                  max = 500,
                  value = 30)
    ),
    sidebarPanel( # input: slider para el numero de repeticiones
      sliderInput(inputId = "n",
                  label = "Número de repeticiones",
                  min = 10,
                  max = 1000,
                  value = 200)
    ),

    # Main panel for displaying outputs ----
    mainPanel(
      # Output: Histogram ----
      plotlyOutput('plot')
    )
  )
)

# Define server logic required to draw a histogram ----
server <- function(input, output) {
  sim <- tlc_viz(n=input$n, sample_size=input$sample_size)
  dominio <- seq(min(sample_means), max(sample_means), length.out=500)
  normal_teo <- dnorm(dominio, mean=sm_avg, sd=sm_sd)
  output$plot <- renderPlotly(
    fig_ly <- plot_ly(x=sample_means, type='histogram', histnorm='probability',)
    fig_ly <- fig_ly %>% add_trace(x=dominio, y=normal_teo, type='scatter')
  )
  
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)