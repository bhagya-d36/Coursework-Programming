#Importing the required libraries
library(ggplot2)
library(dplyr)
library(tidyr)

# Note : xstar will be used instead of x* in code cells

#Defining the probability density function f(x)
pdf <- function(x) 
{return(0.5 * exp(-abs(x)))}


#Assigning values for 'x0','N', and 's'.
x0 <- 0
N <- 10000
s <- 1


#Creating a list to store the generated samples, starting with the initial value x0.
gen_list <- c(x0)

#Mean (xi-1) is the last generated sample and the standard deviation is s.
#Therefore Mean (xi-1) = generated_samples[-1]
#Generating a random number x* with mean (Xi-1) and standard deviation s, for i=1,...,N
for (i in 1:N) 
{   
  xstar <- rnorm(1, mean = gen_list[length(gen_list)], sd = s)
  xstar
  
  r <- pdf(xstar) / pdf(gen_list[length(gen_list)])
  log_r <- log(r)
  u <- runif(1)
  log_u <- log(u)
  
  if (log_u < log_r) { gen_list <- c(gen_list, xstar) } 
  else { gen_list <- c(gen_list, gen_list[length(gen_list)]) }
}

#Creating a data frame with generated samples for plotting purposes
df <- data.frame(gen_list)

#Plotting the histogram, kernel density estimate, and f(x)
ggplot(df, aes(x = gen_list)) +
  geom_histogram(bins = 50, fill = 'darkblue', color = 'black', alpha = 0.5, aes(y = after_stat(density))) +
  geom_density(color = 'blue', linewidth = 1.5) +
  stat_function(fun = pdf, color = 'green', linetype = 'dashed', linewidth = 1.2) +
  labs(title = 'Graphical Representation (Random Walk Metropolis)',
       x = 'x',
       y = 'Probability Density') +
  theme(axis.text = element_text(color = 'black', size = 8),
        axis.title = element_text(color = 'black', size = 8),
        plot.title = element_text(hjust = 0.5, size = 10),
        panel.grid.major = element_line(color = 'gray', linetype = 'dotted'),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = 'white')) 