library(shiny)
library(readr)
library(docstring)
library(ggplot2)
library(plotly)


theory_intro <- "En la teoría de probabilidades, el teorema del limite central (TLC) dice que, cuando sumamos variables aleatorias independientes,
sus sumas tienden a una distribución normal (una campana gaussiana) incluso si las variables originales no tienen distribución normal.
Este teorema es clave en la teoría de probabilidades dado que implica que los métodos estocásticos que funcionan para distribuciones normales son aplicables
a problemas que involucran otro tipo de distribuciones."

theory_math <- "Formalmente, sea $ \\{X_1,\\ldots ,X_n\\}$ una muestra i.i.d de tamaño $n$ con media $\\mu$ y varianza $0< \\sigma^2 < \\infty$. 
Sea $S_n = \\sum\\limits_{i=1}^n X_i$, entonces
$$ \\lim\\limits_{n \\rightarrow \\infty} P \\Big( \\frac{S_n - n \\mu}{\\sigma \\sqrt{n}} \\leq z \\Big) = \\Phi(z)$$
Donde $ \\Phi$ representa la función de distribución acumulada de una $ \\mathcal{N}(0,1)$."

sample_dist_disclaimer <- "En estadística, la distribución binomial es una distribución de probabilidad discreta que cuenta el número de éxitos en una secuencia de $n$ ensayos de Bernoulli independientes entre sí, con una probabilidad fija $p$ de ocurrencia del éxito entre los ensayos. Un experimento de Bernoulli se caracteriza por ser dicotómico, esto es, sólo tiene dos resultados posibles. A uno de estos se denomina «éxito» y tiene una probabilidad de ocurrencia $p$ y al otro, «fracaso», con una probabilidad $q = 1 - p$. En la distribución binomial el experimento se repite $n$ veces, de forma independiente, y se trata de calcular la probabilidad de un determinado número de éxitos.
En nuestro caso elgimos una distribución binomial ya que sabemos que si uno utiliza un p muy chico(cercano a cero) o muy grande (cercano a uno) las distribuciónes originales resultantes se alejan mucho de una normal y resulta interesante ver que así y todo sus sumas tienden en distribución a una normal."

video_intro <- "Este setting tiene la particularidad de que se puede construir un modelo físico para el caso $p = \\frac{1}{2}$, que se conoce como Galton board (tablero de Galton):"

# Define UI for application that plots random distributions 
ui <- shinyUI(fluidPage(
  
  # Application title
  titlePanel("Ciencia de datos"),
  
  # Sidebar with a slider input for number of observations
  sidebarLayout(
    sidebarPanel(
      sliderInput(
        "obs",
        "N:",
        min = 2,
        max = 2000,
        value = 10,
        animate=TRUE
      ),
      sliderInput(
        "sample_size",
        "Sample Size:",
        min = 2,
        max = 100,
        value = 21
      ),
      sliderInput(
        "p",
        "Probabilidad de éxito:",
        min = 0,
        max = 1,
        value = 0.5
        ),
    ),
    
    
    # Show a plot of the generated distribution
    mainPanel(
      
      tabsetPanel(
        id='tabset',
        tabPanel(
          "TLC",
          plotlyOutput("distPlot"),
          HTML('<br><br>'),
          withMathJax(theory_intro),
          HTML('<br><br><br>'),
          withMathJax(theory_math)
        ),
        tabPanel(
          "Sampling Distribution",
          plotlyOutput("samplehist"),
          withMathJax(sample_dist_disclaimer),
          HTML('<br><br><br>'),
          withMathJax(video_intro),
          HTML('<br><br>'),
          HTML('<iframe width="560" height="315" src="https://www.youtube.com/embed/EvHiee7gs9Y" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>')
        )
      )        
      

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

  
  tcl_viz <- function(n=300, sample_size=21, p=0.5) {
    
    sim <- rbinom(n, sample_size, p)
    m <- matrix(rbinom(n*sample_size, sample_size, p), ncol=sample_size)
    # TODO: (nice to have) Agregar una solapa con un histograma de as.vector(m)
    sample_means <- rowMeans(m)
    dens <- density(sample_means)
    
    sm_avg <- mean(sample_means)
    sm_sd <- sd(sample_means)
    analytical_sd <- sqrt(sample_size*(p-p^2)/sample_size)
    
    dominio <- seq(min(sample_means), max(sample_means), length.out=500)
    normal_teo <- dnorm(dominio, mean=sample_size*p, sd=analytical_sd)
    
    hist_df <- as.data.frame(sample_means)
    
    normal_teo_df <- with(hist_df, data.frame(x=dominio, y=normal_teo))
    fig <- ggplot(hist_df,aes(x=sample_means)) +
      geom_histogram(aes(y=..density..),
                     bins=sample_size, fill="grey", color="black") + 
      geom_density(aes(colour='dodgerblue'), alpha=.8 ) +
      
      geom_line(data=normal_teo_df, aes(x=x, y=y,color="red"))+
      scale_colour_manual(name='Lineas:',
                          values=c("dodgerblue",'red'),
                          labels=c('Density','Normal'))+
      theme(legend.title = element_blank(), legend.justification = c(1, 1), legend.position = c(1, 1), )

  }
  
  make_samp_hist <- function(n=300, sample_size=21, p=0.5) {
    sim <- rbinom(n, sample_size, p)
    m <- matrix(rbinom(n*sample_size, sample_size, p), ncol=sample_size)
    sh_df <- as.data.frame(as.vector(m))
    colnames(sh_df) <- 'muestras'
    
    ggplot(sh_df,aes(x=muestras)) + geom_histogram(aes(y=..density..),
                                                  bins=sample_size, fill="grey", color="black")
  }
  
  
  output$samplehist <- renderPlotly({
    
    g <- make_samp_hist(input$obs,input$sample_size, input$p)
    print(ggplotly(g, tooltip=c("name", "density", "samples") ))
    
  })
  
  output$distPlot <- renderPlotly({
    
    
    g <- tcl_viz(input$obs,input$sample_size, input$p) # Grafico ggplot2
    
    pg <- ggplotly(g) # grafico convertido a plotly graph
    # rename label legends
    pg$x$data[[2]]$name <- "Density"
    pg$x$data[[3]]$name <- "Normal"
    
    # generate an rnorm distribution and plot it
    print(pg %>% 
            layout(
                   legend = list(orientation = "v",
                                 bgcolor = 'rgba(0,0,0,0)', ncol=2,
                                 y = 1, x = 0)))
  })
  
})


# Create Shiny app ----
shinyApp(ui = ui, server = server)
