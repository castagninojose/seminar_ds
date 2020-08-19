library(shiny)
library(readr)
library(docstring)
library(ggplot2)
library(plotly)


# Define UI for application that plots random distributions 
ui <- shinyUI(fluidPage(
  
  # Application title
  titlePanel("Ciencia de datos"),
  
  # Sidebar with a slider input for number of observations
  sidebarLayout(
    sidebarPanel(  # Aca van inputs, sliders etc.
      sliderInput("obs",  # ID
                  "N:",  # Etiqueta
                  min = 1, 
                  max = 500, 
                  value = 300)
      ,
      sliderInput("sample_size", 
                  "Sample Size:", 
                  min = 10, 
                  max = 1000, 
                  value = 21)
      ,
      sliderInput("bin", 
                  "Bins:", 
                  min = 5, 
                  max = 30, 
                  value = 10)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotlyOutput("distPlot"),
      p("* El density no es la normal que queremos mostrar, aunque no se bien todavia como agregarselo al histograma de ggplot", style = "color:red"),
      p("* Revisar bien rangos y que es cada valor de los sliders")
    )
  )
))

# Define server logic required to generate and plot a random distribution
server <- shinyServer(function(input, output) {
  
  # Funcion que ahora devuelve un objeto ggplot 
  tcl_viz <- function(n=300, sample_size=21, bins=10) {
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
    
    #fig <- plot_ly(x=sample_means, type = 'histogram', histnorm = 'probability')
    
    
    #fig <- fig %>% add_trace(x=dominio, y=normal_teo, type='scatter')
    #fig <- fig %>% add_trace(x=sample_means, y=density, type='scatter')
    
    fig <- ggplot(as.data.frame(sample_means), aes(x=sample_means)) + 
      geom_histogram(aes(y=..density..),bins=bins, colour="blue", fill="dodgerblue") +
      geom_density(alpha=.8, colour='red') + stat_function(fun=dnorm, args = list(mean=sm_avg,sd=sm_sd))#  Esta es la density de los datos, no se bien aun como agregar la otra (normal_teo)

  }
  
  output$distPlot <- renderPlotly({
    
    print(ggplotly(tcl_viz(input$obs,input$sample_size, input$bin))) # Como argumenos va input$ mas el id del input de la parte de server
  })
  
})

shinyApp(ui = ui, server = server)
