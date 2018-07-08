# EM ALGORITHM for GAUSSIAN MIXTURES
# << SERVER >>
# by Aydin Mohseni


### Install packages
library(shiny)
library(shinyjs)
library(ggplot2)
library(mvtnorm)
library(MASS)
library(ellipse)

### Define server logic for Application
shinyServer(function(input, output, session) {
  
  
  ### 1. Load the selected data set
  chooseData <- reactive({
    if (input$DataSet == "Data Set 1") {
      data <- data.frame(read.table("data/dataset1.txt"))
    }
    if (input$DataSet == "Data Set 2") {
      data <- data.frame(read.table("data/dataset2.txt"))
    }
    if (input$DataSet == "Data Set 3") {
      data <- data.frame(read.table("data/dataset3.txt"))
    }
    # output data to be accessed by other reactive contexts
    return(list(data))
  })

  
  ### 2. Render the initial plot of the data
  output$distributionPlot <- renderPlot({
    # access the selected data set
    df <- chooseData()[[1]]
    # plot data
    ggplot(data = df, aes(V1, V2)) + geom_point() +
      labs(title = "", x = "X", y = "Y") +
      theme_light() +
      theme(
        plot.title = element_blank(),
        axis.title.x = element_text(size = 16),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_text(size = 16),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()
      )
  })

  
  ### 3. Run the EM algorithm on the data set
  # Set algorithm parameters, define functions, and iterate until convergence
  doEMAlgorithm <- reactive({
    # Run (take a depdendency) with runSimulation action button pressed
    input$runAlgorithm

      # Load the currently selected data set, and format it
      df <- chooseData()[[1]]
      df <- df[sample(nrow(df)),] # Randomize rows
      colnames(df) <- c("x1", "x2")

      # Set the algorithm parameters
      K <-
        input$NumberOfComponents # Number of underlying components k=1,..,K
      N <- nrow(df) # Number of data vectors x_i
      d <- ncol(df) # Dimension of data vectors x_i
      t <- 0 # Timer initialized at 0
      converged <- FALSE # Convergence status initialized

      # Create data frames in which to store relevant computations:
      # Component mean vectors μ_k (k=1,..,K)
      kMean <- data.frame(matrix(
        data = NA,
        nrow = K,
        ncol = d
      ))
      # Covariance matrix Σ for whole data set
      covMatrix <- data.frame(matrix(
        data = NA,
        nrow = d ,
        ncol = d
      ))
      # Marginal probabilities α_k vector of components z_k (1,..,K)
      ALPHA <- rep((1 / K), K)
      # Matrix of membership weights w_ik of data vectors x_i (i=1,..,N) in components z_k (k=1,..,K)
      W <- data.frame(matrix(
        data = (1 / K),
        nrow = N,
        ncol = d
      ))
      # Vector in which to record the log-likelihood as a function of the number of iterations
      LLHt <- c()
      # Set up color vector for each component z_k distribution
      graphColor <- c("red", "blue", "green", "orange", "purple")
      # Set up the 95% connfidence interval ellipse data frame
      EllipsesDf <- data.frame(matrix(
        data = NA,
        nrow = K * 100 * 100, 
        ncol = 4
      ))
      colnames(EllipsesDf) <- c("x","y","group", "iteration")
      
      # FUNCTIONS
      # Compute the LIKELIHOOD Pr(x_i|z_ik=1,θ_k):
      # the probability of a data vector x_i (i=1,..,N) given component z_k (k=1,..,K) and component parameters θ_k.
      LH <- function(i, k) {
        MEAN <-
          as.numeric(kMean[k, ]) # Find the component z_k mean vector μ_k
        SIGMA <-
          as.matrix(eval(parse(text = paste(
            "covMatrix", k, sep = ""
          )))) # Find the component z_k covariance matrix Σ_k
        w <-
          dmvnorm(as.numeric(df[i, ]), mean = MEAN, sigma = SIGMA) # Calculate the probability from the multi-variate normal
        return(w)
      }

      # Compute α_k: the MARGINAL (unconditional) probability of a component z_k (k=1,..,K)
      MARG <- function(k) {
        return(mean(W[, k]))
      }

      # Compute the NORMALIZING constant ΣPr(x_i|z_ik=1,θ_k)·α_k:
      # The sum of the likelihoods of x_i given the different z_k (1,..,K) weighted by the marginals α_k
      NORM <- function(i) {
        return(sum(mapply(LH, i, 1:K) * ALPHA))
      }
      # Compute the POSTERIOR Pr(z_ik=1|x_i,Θ):
      # that is, the probability of a vector belonging to a component given that data vector x_i and the component parameters θ_k.
      POST <- function(i, k) {
        (LH(i, k) * ALPHA[k]) / NORM(i)
      }

      # The DIFFERENCE function used to calculate COVARIANCE matrices Σ_z
      DIF <- function(i, k) {
        # Calculate the difference of the data vector x_i from mean vector μ_k
        x <-
          as.numeric(sweep(df[i, ], 2, as.numeric(kMean[k, ]), "-"))
        # Return the matrix product of this difference vector multiplied by its own transpose
        return(x %*% t(x))
      }

      # Compute the COVARIANCE matrix Σ_z for the whole data set
      COV <- function(x) {
        y <- data.frame(matrix(
          data = NA,
          nrow = ncol(x),
          ncol = ncol(x)
        ))
        for (i in 1:ncol(x)) {
          for (j in 1:ncol(x)) {
            y[i, j] <- cov(x[, i], x[j])
          }
        }
        return(y)
      }

      ### ALGORITHM
      
      # Progress bar message
      withProgress(message = 'Generating plot', value = 0, {
        
      # Set the E-M algorithm to repeat until convergence is attained
      repeat {
        if (converged == TRUE) {
          # If the algorithm has converged,
          # output results to be accessed by the plots 
          # return(list(EllipsesDf,
          #             LLHt))
          # and then signal convergence & end the algorithm
          break

        } else {
          if (t == 0) {
            # Step 1: (Initialize) If this is the first round,
            # then randomly select K data vectors x_i (i=1,..,N)
            # to act as the initial mean vectors μ_k (k=1,..,K) for each of the K components
            for (i in 1:K) {
              kMean[i, ] <- df[sample(1:N, 1), ]
            }

            # Initialize each of the K component covariance matrices
            # as the covariance matrix of the whole data set
            for (k in 1:K) {
              assign(paste("covMatrix", k, sep = ""), COV(df))
            }
            # and their corresponding correlation matrices
            # (for graphing mean-covariance ellipses)
            for (k in 1:K) {
              w <- as.matrix(eval(parse(
                text = paste("covMatrix", k, sep = "")
              )))
              assign(paste("corMatrix", k, sep = ""), cov2cor(w))
            }

            # INITIAL plot of distribution means & covariances
            # Plot the x-vector data

            # Increment iteration
            t <- t + 1

          } else {
            # Step 2: (E-Step) Compute the log-likelihood w.r.t. Pr(z_i|x_i,Θ)
            # Compute membership weights w_ik of x_i in k, given Θ
            for (i in 1:N) {
              for (k in 1:K) {
                W[i, k] <- POST(i, k)
              }
            }

            # Step 3: (M-Step) Given the membership weights obtained from the E-Step,
            # we can calculate the new parameters Θ.
            # Specifically:
            # i. marginals α_k,
            # ii. mean vectors μ_k,
            # iii. and covariance matrices Σ_k
            # for k=1,..,K.

            # Calculate the new marginal (unconditional) probabilities α_k
            ALPHA <- mapply(MARG, 1:K)
            Nk <- ALPHA * N

            # Calculate the new component mean vectors μ_k
            for (k in 1:K) {
              kMean[k,] <- (1 / Nk[k]) * colSums(W[, k] * df)
            }

            # Calculate the new covariance matrices Σ_k
            for (k in 1:K) {
              u <-
                list() # Create a list of covariances for every data vector x_i for each component k_i
              for (i in 1:N) {
                # Calculate and store each w_ik weighted matrix of squared differences of the x_i-k_i pairs
                u[[i]] <- W[i, k] * DIF(i, k)
              }
              # Take the element-wise sum of the resulting 2x2 matrices and normalize them by 1/N_k
              v <- (1 / Nk[k]) * Reduce('+', u)
              # Name and save the new covariance matrix Σ_k for k=(1,..,K)
              assign(paste("covMatrix", k, sep = ""), v)
            }

            # Plot the means & covariances for the clusters
           
            # Plot the ellipses representing the mean & variances of the distributions

            # Step 4: (Convergence)
            # Calculate the log-likelihood
            # log(l(Θ))=Σlog(Pr(x_i|Θ))=Σ(log(Σα_k*Pr(x_i|z_k,Θ_k)))
            LLHn <- c()
            for (i in 1:N) {
              LLHnk <- c()
              for (k in 1:K) {
                LLHnk[k] <- LH(i, k) * ALPHA[k]
              }
              LLHn[i] <- log(sum(LLHnk))
            }
            LLH <- sum(LLHn)

            # Save the log-likelihood for this iteration in a data frame
            LLHt[t] <- LLH

            # INITIAL plot of distribution means & covariances
            # Plot the x-vector data

            # Plot the ellipses representing the mean & variances of the distributions

            # Check for convergence:
            # If the number of iterations is at least 2,
            # and the difference in the log-likelihood has changed by less than 1
            # then say the algorithm has converged
            if (t > 1) {
              if (LLHt[t] - LLHt[t - 1] < .05) {
                converged <- TRUE
              }
            }
            # Otherwise, increment the iteration counter,
            # and repeat the E & M steps.
            if (converged == FALSE) {
              
              # Increment the progress bar, and update the detail text.
              incProgress(1 / 20, detail = paste("Running step", t))
              
              t <- t + 1
            }
          }
        }
      }
    })
  })

  # ### Plot the data plot, and current Gaussian distributions
  # output$distributionPlot <- renderPlot({
  #
  #   # Load the currently selected data set, and format it
  #   df <- chooseData()[[1]]
  #   colnames(df) <- c("x1", "x2")
  #
  #   # Load the current means and variances of the distributions
  #
  #   # Plot the means & covariances for the clusters
  #
  #
  # }, height = 400, width = 400)
  #
  ### Plot the current model score
  # output$scorePlot <- renderPlot({
  #
  #   # Load the relevant data
  #   LLHt <- EM.Algorithm()[[1]]
  #
  #   # Compile the log-likelihood values for each iteration
  #   # into a single matrix, along with interation indices
  #   LLHgraph <- data.frame(c(1:t), LLHt)
  #   colnames(LLHgraph) <- c("Iteration", "LogLikelihood")
  #
  #   # Plot the log-likelihood of the model as a function of iterations
  #   ggplot(LLHgraph, aes(x = Iteration, y = LogLikelihood)) +
  #     geom_line() + ggtitle("Log-Likelihood as a Function of Iterations") +
  #     labs(y = "Log-Likelihood log-l(Θ)", x = "Iteration t")
  #
  # }, height = 400, width = 400)

  ### 3. Swap static plot for animation
  # observeEvent(input$runAlgorithm, {
  #   hide("distributionPlot")
  #   show("algorithmAnimation")
  # })
  
})

### EOD ###