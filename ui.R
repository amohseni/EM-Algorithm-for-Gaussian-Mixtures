# EM ALGORITHM for GAUSSIAN MIXTURES
# << UI >>
# by Aydin Mohseni


# Load the shiny GUI library
library(shiny)

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
        "Data set:",
        c("Data Set 1", "Data Set 2", "Data Set 3"),
        selected = "Data Set 3"
      ),
      
      sliderInput(
        "NumberOfComponents",
        "Number of distributions:",
        min = 1,
        max = 5,
        value = 2,
        step = 1
      ),
      
      p(actionButton("runAlgorithm", "Run Algorithm"), align = "center")
      
    ),
    
    # Main panel with plot outputs
    mainPanel(
      plotOutput(outputId = "distributionPlot")
    )
    
    # column(
    #   width = 3,
    #   plotOutput(outputId = "scorePlot")
    # ))
  )
))
