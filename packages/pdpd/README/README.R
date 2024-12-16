# README

set.seed(2187)
n <- 100 # Number of observations
x <- matrix(runif(n*10), nrow = n, ncol = 10) # Matrix of predictors
colnames(x) <- paste0('x',1:ncol(x))

# True predictive function (Friedman 1991)
f <- function(x) 10*sin(pi*x[,1]*x[,2]) + 20*(x[,3]-.5)^2+10*x[,4]+5*x[,5]

# Simulate outcome
y <- rnorm(n, f(x))

# Fit BART model and request 500 posterior samples
library(BART)
bartFit <- wbart(x, y, nskip = 500, ndpost = 500)
saveRDS(bartFit, 'README/bartFit.rds')

# Create estimated predictive function
f_hat <- function(x) {

  # Make predictions (and prevent excessive printing) from the BART package
  capture.output(preds <- t(predict(bartFit, x)))

  return(preds)
}

library(pdpd)
pd <- bayes_pd(x = x,         # training data
               f_hat = f_hat, # estimated predictive function
               vars = 'x1',   # predictor to examine
               k = 40,        # number of points to evaluate at
               limits = c(0.025, 0.975), # posterior credible interval limits
               f = f) # optionally, true predictive function for comparison


# Plot
png("README/pdx1.png", width = 800, height = 600)
plot(pd$est ~ pd$x1, type = 'l', ylim = c(min(pd$lcl), max(pd$ucl)))
lines(pd$lcl ~ pd$x1, type = 'l', lty = 2)
lines(pd$ucl ~ pd$x1, type = 'l', lty = 2)
lines(pd$truth ~ pd$x1, type = 'l', col = 'red')
dev.off()
