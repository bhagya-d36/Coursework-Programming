#Importing the required libraries
library(ggplot2)
library(dplyr)

#Note: Rhat will be used to represent the R̂ diagnostic.

#Taking N = 2000, s = 0.001, and J = 4
N <- 2000
s <- 0.001
J <- 4
#Array containing 200 s values equally spaced between 0.001 and 1 (inclusive)
s_list <- seq(0.001, 1, length.out = 200)

#Defining the function calc_R̂ under the parameters (N,s,J)
calc_Rhat <- function(N, s, J) {
  chains_list <- list()  # A list to store the generated chains
  
  #Generating J chains
  for (i in 1:J) {
    x <- rnorm(1)  #Generating random number x from standard normal distribution
    chain <- c(x)  #Chain with initial value as x
    
    #Generating N values for the chain
    for (j in 2:N) {
      
      #Generating a new value n from normal distribution with mean x and std_dev s
      n <- rnorm(1, mean = x, sd = s)
      
      #Calculating acceptance probability A where A = min(1, f(x(n)) / f(x(x)))
      A <- min(1, exp(-0.5 * (n^2 - x^2)))
      
      #If condition to accept or reject the new value n
      if (runif(1) < A) {
        x <- n
      }
      
      #Append x to the chain after accepting/rejecting n
      chain <- c(chain, x)
    }
    
    #Adding the completed chain to the list of chains
    chains_list[[i]] <- chain
  }
  
  #Rhat calculation procedure when N=2000, s=0.001, and J=4.
  
  Mj <- sapply(chains_list, mean)  #Sample mean of chain j
  
  Vj <- sapply(chains_list, var)   #Within sample variance of chain j
  
  W <- mean(Vj)                     #Overall within sample variance
  
  M <- mean(Mj)                     #Overall sample mean
  
  B <- mean((Mj - M)^2)             #Sample variance
  
  Rhat <- sqrt(B + W / W)           
  
  return(Rhat)
}

#Rhat calculation
Rhat <- calc_Rhat(N, s, J)
cat('Rhat:', Rhat, '\n')

#Calculating the variation of Rhat for s_list
Rhat_values <- sapply(s_list, function(s) calc_Rhat(N, s, J))
#Creating a dataframe for plotting purposes
df <- data.frame(s = s_list, Rhat = Rhat_values)

#Graphical representation of the variation of Rhat over s
#Note: Rhat will be used to represent R̂ on the graph
ggplot(df, aes(x = s, y = Rhat)) +
  geom_line(color = 'navy', linewidth = 1) +
  labs(x = 's', y = 'Rhat', title = 'Graphical Representation (Rhat vs s)')
