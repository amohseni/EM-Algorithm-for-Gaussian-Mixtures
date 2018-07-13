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
  
  fluidRow(
    style = "background-color:#F2F2F2; margin-top: 30px; margin-bottom: 30px; padding: 10px", 
    column(
      width = 4,
      # Introduction text:
      p(
        tags$b("Desciption:"),
        "A fundamental problem in inductive inference is how to learn the structure of unabeled data.
        This is known as unsupurvised learning in machine learning, and clustering in statistics.",
        br(), br(),
        "The estimation-maximization (EM) algorithm is an important tool for such problems
        as it allows for an iterative approach to a maximum likelihood solution."
      )
    ),
    column(
      width = 4,
      # Introduction text:
      p(
        "Here, we assume that there are \\(K\\)  Gaussian components, 
        and we attempt to learn their means and covariances.
        The EM is initialized at random, then proceeds by alternating between assessing the log-likelihood of the data given the parameters of the model (the E step) 
        and updating the parameters of the model to maximize the likelihood of that expectation (the M step).
        The updated parameters are then used in the next E step.
        The algorithm converges when gains in log-likehood become negligible."
      )
    ),
    column(
      width = 4,
      p(
        tags$b("Instructions:"),
        "Select the data set, number of components \\(K\\)  for the model, 
        and confidence interval \\((1-\\epsilon)\\)  to be displayed, then select 'RUN ALGORITHM'.", 
        br(), br(), 
        "After running the algorithm, 
        you can play the animations for the evolution of the component parameter estimates 
        and the log-likelihood score of the model."
      )
    )
  ),
  
  # Sidebar with controls
  sidebarLayout(
    sidebarPanel(
      width = 4,
      
      selectInput(
        "DataSet",
        "Choose a data set:",
        c("Data Set 1", "Data Set 2", "Data Set 3"),
        selected = "Data Set 3"
      ),
      
      plotOutput("distributionPlot", height = "100px"),
      
      br(),
      
      fileInput('uploadedFile', 'Or upload your own file:',
                accept = c(
                  'text/csv',
                  'text/comma-separated-values',
                  'text/tab-separated-values',
                  'text/plain',
                  '.csv',
                  '.tsv'
                ),
                placeholder = "    No file selected"
      ),
      tags$hr(),
      p('You can upload any comma-separated CSV, TSV, or TXT file
        where the X and Y values are contained in the table columns.'),
      tags$hr(),
      
      sliderInput(
        "NumberOfComponents",
        "Number of components \\(K\\):",
        min = 1,
        max = 5,
        value = 2,
        step = 1
      ),
      
      sliderInput(
        "ConfidenceInterval",
        "Confidence interval \\((1-\\epsilon)\\):",
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
      plotlyOutput("scorePlot", height = "300px")
    )
  )
))
