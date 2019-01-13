
# we observe that our data is normal distribution with mean 5 and standard deviation 2

y <- rnorm(50)
x <- rnorm(50)

mu <- 5
sigma <- 2


simul_model <- function(size_data = 50, numofsim = 1000) {
  x <- vector(mode = "list", length = numofsim)
  y <- vector(mode = "list", length = numofsim)
  for( i in 1:numofsim) {
    x[[i]] <- rnorm(size_data, 5, 2)
    y[[i]] <- 0.5 + 0.3*x[[i]] + rnorm(size_data, 0, 1)
    
  }
  list(x, y)
}

sd(simul_model(numofsim = 100)[[1]][[1]])


