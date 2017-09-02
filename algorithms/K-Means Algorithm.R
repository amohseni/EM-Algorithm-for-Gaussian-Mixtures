# CS 274A | Homework 6
# K-Means Algorithm
# by Aydin Mohseni

# Install Packages
install.packages("ggplot2")
library(ggplot2)

# Set the working directory, from which to upload data:
FILE <- file.choose()
DIR  <- dirname(FILE)
setwd(DIR)
getwd()

# Import DataSet 1
DATA <- data.frame(read.table("dataset2.txt"))

# Import data & assign basic data variables
df <- DATA # Choose data set
df <- df[sample(nrow(df)),] # Randomize rows
colnames(df) <- c("x1", "x2")
K <- 3 # Number of underlying components k=1,..,K
N <- nrow(df) # Number of data vectors x_i
d <- ncol(df) # Dimension of data vectors x_i
t <- 0 # Timer initialized at 0
converged <- FALSE # Convergence status initialized

# Create data frames in which to store relevant computations
kMean <- data.frame(matrix(data = NA, nrow=K, ncol=d)) # Component mean vectors
NkAssign <- c() # Data vector component assignments
kEdist <- matrix(data = NA, nrow = N, ncol = K)  # Euclidean distance of data vectors x_i from component mean vectors μ_k
kEdistComp <- matrix(data = NA, nrow = N, ncol = 1)  # Euclidean distance of data vectors x_i from component mean vectors to which they are assigned

# Create the mean squared-error function
MSE <- function(x, y) {
  z <- c()
  for (i in 1:length(x)) {
    z[i] <- (x[i]-y[i])^2
  }
  return(mean(z))
}

# Set the k-means algorithm to repeat until convergence is attained
repeat {
  if (converged == TRUE) {

    # Signal Convergence
    print(paste("Converged in round ", t, sep = ""))

    break
  } else {

    if (t == 0) {

      # Step 1: (Initialize) If this is the first round,
      # then randomly select K data vectors x_i (i=1,..,N)
      # to act as the initial mean vectors μ_k (k=1,..,K) for each of the components z_k (k=1,..,K)

      for(i in 1:K) {
        kMean[i,] <- df[sample(1:N, 1),]
      }
      print("Initial component mean vectors μ_k (k=1,..,K)")
      print(kMean)

      # Increment iteration
      t <- t+1

      } else {

      # Step 2: If this is round t>0,
      # then assign each of the N data vectors x_i (i=1,..,N)
      # to the component z_k (k=1,..,K) with the nearest mean μ_k (k=1,..,K)
      # in terms of Euclidean distance in the d-dimensional input space

      # Compute the Euclidian distance for each data vector x_i (i=1,..,N)
      # from each component mean vector μ_k (k=1,..,K)

      for(i in 1:N) { # For each data vector x_i (i=1,..,N)

        for(k in 1:K) { # And each component mean vector μ_k (k=1,..,K)
          # Compute the distance of the data vector x_i
          # from the component mean vector μ_k,
          # and store them in the distance matrix
          kEdist[i,k] <- MSE(df[i,], kMean[k,])
        }

        # Select the index of the component with the closest mean vector
        # And assign the data vector to that component
        NkAssign[i] <- which(kEdist[i,] == min(kEdist[i,])[1])
      }

      # Step 3: For each of the components z_k (k=1,..,K), compute the new mean vector μ_k

      for(k in 1:K) {

        # Get the indices for the data vectors x_i assigned to each component z_k
        kMembers <- which(NkAssign == k)

        for(j in 1:d) {

          # Compute the new mean vector μ_k of each component z_k (k=1,..,K)
          # as the mean of its assigned data vectors
          kMean[k, j] <- mean(df[kMembers,j])

        }
      }

        # Store the assignment vector in a data frame
        assign(paste("NkAssign", t, sep = ""), NkAssign)

        # Store the mean vectors in a data frame
        assign(paste("kMean", t, sep = ""), kMean)

        # Print the new mean vectors
        print(paste("Iteration = ", t, sep = ""))
        print("New mean vectors")
        print(eval(parse(text=paste("kMean", t, sep=""))))

        # Store the overall mean squared-error of the model
        for (i in 1:N) {
          # In particular, the distance of each vector x_i
          # from its assigned component mean vector μ_k
          kEdistComp[i,] <- kEdist[i,NkAssign[i]]
        }
        assign(paste("kEdist", t, sep = ""), mean(kEdistComp))

        # PRINT a plot of the data vector assignments to-date
        graphData <- cbind(df, NkAssign)
        colnames(graphData) <- c("x1","x2","K")
        p <- ggplot(graphData, aes(x1, x2)) +
          geom_point(aes(colour = factor(K))) +
          ggtitle(paste("K-Means Algorithm After ", t, " Iterations", sep="")) +
          labs(x="X", y="Y", color= "Component")
        print(p)

      # Step 4: Check for convergence.
      # If none of the data vectors have changed their assigned component,
      # the algorith has converged, and can halt.
      # If not, repeated steps 2 and 3.

        if (t>1) {

          # Check if the assignment matrix is the same as before,
          # and if so, record that the process has converged
          if (identical(eval(parse(text=paste("NkAssign", t, sep = ""))),
                        eval(parse(text=paste("NkAssign", (t-1), sep = ""))))) {

            converged <- TRUE

            } else {

              # If the assignment are not the same,
              # then the process has not converged,
              # continue to repeat steps 2 & 3.

            }
        }

        # If the algorithm has not converged,
        # then increment the iteration counter,
        if (converged == FALSE) { t <- t+1 }
    }
  }
}

# Compile the mean squared-error values for each iteration
# into a single matrix, along with interation indices

MLEgraph <- data.frame(matrix(data = NA, nrow = t, ncol = 2))
MLEgraph[,1] <- c(1:t) # Itereration indices
colnames(MLEgraph) <- c("Iteration", "MSE")

for (i in 1:t) { # Compiline the MSE values
  MLEgraph[i,2] <- eval(parse(text = paste("kEdist", i, sep = "")))
}

# Plot the mean squared error of the model as a function of iterations
q <- ggplot(MLEgraph, aes(x = Iteration, y = MSE)) +
  geom_line() + ggtitle("Means Squared-Error as a Function of Iterations") +
  labs(x = "Iteration t", y = "Mean Squared-Error (MSE)")
print(q)

