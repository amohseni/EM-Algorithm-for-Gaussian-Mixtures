# EM ALGORITHM for GAUSSIAN MIXTURES
# << UI >>
# by Aydin Mohseni


# Load the shiny GUI library
library(shiny)
library(shinyjs)

# Set encoding for special characters
Sys.setlocale("LC_ALL", "fr_FR.UTF-8")

# Define UI for application
shinyUI(fluidPage(
  # CSS for visual
  includeCSS("www/style.css"),
  
  # Load Shinyjs for showing/hiding elements
  useShinyjs(),
  
  # Load MathJax for equations
  withMathJax(),
  
  # Main title
  titlePanel("EM Algorithm for Gaussian Mixtures"),
  
  # Sidebar with controls
  sidebarLayout(
    sidebarPanel(
      width = 3,
      
      selectInput(
        "DataSet",
        "Input data:",
        c("Data Set 1", "Data Set 2", "Data Set 3"),
        selected = "Data Set 3"
      ),
      
      plotOutput("distributionPlot", height = "100px"),
      
      br(),
      
      sliderInput(
        "NumberOfComponents",
        "Number of components:",
        min = 1,
        max = 5,
        value = 2,
        step = 1
      ),
      
      sliderInput(
        "ConfidenceInterval",
        "Confidence Interval:",
        min = 0.90,
        max = 0.99,
        value = 0.95,
        step = 0.01
      ),
      
      # Run algorithm button
      p(actionButton("runAlgorithm", "Run Algorithm"), align = "center")
      
    ),
    
    # Main panel with plot outputs
    mainPanel(
      plotlyOutput("animatedDistributionPlot"),
      br(),
      plotOutput("scorePlot", height = "200px")
    )
  )
))
