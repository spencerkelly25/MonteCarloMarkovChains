#MCMC Metropolis Sampler 
library(ggplot2)
library(gridExtra)
library(dplyr) 

#Change the value of n to see results of different sample sizes! 
n <- 500                                     # Setting up Proposed Normal Distribution
mean <- 100
var <- (15)
normal_dist <- rnorm(n <- n, mean <- mean, var <- var)
normal_df <- data.frame(normal_dist)

samples <- numeric(n)                        # 500 Samples 
samples[1] <- 110                            # The initial guess 
for (i in 2:n) 
{ 
  proposal <- samples[i-1] + rnorm (1, 0, 5) # Proposal Value
  if ((dnorm (proposal, 100, 15) / dnorm (samples[i-1], 100, 15)) > runif (1))
  samples[i] <- proposal                     # Accept Proposal 
  else (samples[i] <- samples[i-1])          # Reject Proposal 
}

df <- data.frame(samples)                    #Setting up Sampled Values Plot 
df_index <- as.numeric(rownames(df))
sample_plot <- ggplot(data <- df, aes(x <- df_index, y <- samples)) +
  geom_line() + 
  xlab('Iteration') + 
  ylab('Sampled Values')

df$normal <- normal_dist                     # Density Plot
density_plot <- ggplot(data <- df, aes(x <- samples)) +
  geom_density() +
  geom_density(aes(x <- normal), linetype = 'dashed') +
  xlab('Density') +
  ylab('Sampled Values')

grid.arrange(sample_plot, density_plot, ncol = 1)

df2 <- sort(df$samples)                      #SSR as scoring metric? 
df2 <- data.frame(df2)
names(df2)[1] <- 'sample'
df2$normal <- sort(df$normal)
df2$resid <- df$sample - df$normal
df2$resid_squared <- df2$resid^2
sum(df2$resid_squared)
