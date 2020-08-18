library(readr)
library(docstring)
library(ggplot2)
library(plotly)
library(shiny)


setwd("/home/puff/git-repos/seminar_ds/entregas/")



tcl_viz <- function(n=300, sample_size=21) {
  pe <- 0.5
  sim <- rbinom(n, sample_size, pe)
  m <- matrix(sim, n)
  
  sample_means <- rowMeans(m)
  dens <- density(sample_means)
  
  sm_avg <- mean(sample_means)
  sm_sd <- sd(sample_means)
  analytical_sd <- sqrt(n*pe*(1-pe)/sample_size)
  
  dominio <- seq(min(sample_means), max(sample_means), length.out=500)
  normal_teo <- dnorm(dominio, mean=sm_avg, sd=sm_sd)
  
  fig <- plot_ly(x=sample_means, type = 'histogram', histnorm = 'probability')
  fig <- fig %>% add_trace(x=dominio, y=normal_teo, type='scatter')
  #fig <- fig %>% add_trace(x=sample_means, y=density, type='scatter')
  fig
}
tcl_viz()


ui <- fluidPage(
  titlePanel("Visualizando TLC"),
  # panel lateral
  sidebarLayout(
    sidebarPanel(
      # input: slider para el tamaño de la muestra
      sliderInput(inputId = "tamaño de la muestra",
                  label = "n:",
                  min = 2,
                  max = 500,
                  value = 30)
    ),
    sidebarPanel( # input: slider para el numero de repeticiones
      sliderInput(inputId = "número de repeticiones",
                  label = "n:",
                  min = 10,
                  max = 1000,
                  value = 200)
    ),

    # Main panel for displaying outputs ----
    mainPanel(
      # Output: Histogram ----
      plotOutput(outputId = "distPlot")
    )
  )
)

# Define server logic required to draw a histogram ----
server <- function(input, output) {
  output$distPlot <- renderPlot({
    x    <- airquality$Ozone
    bins <- seq(min(x), max(x), length.out = input$repeticiones + 1)
    hist(x, breaks = bins, col = "#75AADB", border = "black",
         xlab = "algo",
         main = "una cosa")
    
  })
  
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)