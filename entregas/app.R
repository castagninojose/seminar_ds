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
    sidebarPanel(
      sliderInput("obs", 
                  "N:", 
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
      p("* En rojo es la normal teorica", style = "color:red"),
      p("* En azul el density del histograma", style = "color:blue"),
      p("* Revisar bien rangos y que es cada valor de los sliders")
    )
  )
))

# Define server logic required to generate and plot a random distribution
server <- shinyServer(function(input, output) {
  
  # Expression that generates a plot of the distribution. The expression
  # is wrapped in a call to renderPlot to indicate that:
  #
  #  1) It is "reactive" and therefore should be automatically 
  #     re-executed when inputs change
  #  2) Its output type is a plot 
  #
  
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
    
    hist_df <- as.data.frame(sample_means)
    
    normal_teo_df <- with(hist_df, data.frame(x = dominio, y =normal_teo))

    fig <- ggplot(hist_df,aes(x=sample_means)) +
      geom_histogram(aes( y = ..density..),bins=bins,
                     binwidth = 1, fill = "grey", color = "black") + 
      geom_density(alpha=.8, colour='dodgerblue') +
      geom_line(data = normal_teo_df, aes(x =x, y = y), color = "red")

    #fig <- plot_ly(x=sample_means, type = 'histogram', histnorm = 'probability')
    #fig <- fig %>% add_trace(x=dominio, y=normal_teo, type='scatter')
    #fig <- fig %>% add_trace(x=sample_means, y=density, type='scatter')
    
    
    fig
    
    
    #fig <- ggplot(as.data.frame(sample_means), aes(x=sample_means)) + 
    #  geom_histogram(aes(y=..density..),bins=bins, colour="blue", fill="dodgerblue")+
    #  geom_density(alpha=.8, colour='red') 

  }
  
  output$distPlot <- renderPlotly({
    
    # generate an rnorm distribution and plot it
    print(ggplotly(tcl_viz(input$obs,input$sample_size, input$bin)))
  })
  
})


# Create Shiny app ----
shinyApp(ui = ui, server = server)

