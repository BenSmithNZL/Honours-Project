## VAR creator ##
library(MASS)

VARMaker <- function(periods, beta, sigma){
  
  #Finding constants
  K <- as.numeric(ncol(beta))
  h <- (nrow(beta) - 1) / K
  
  #Creating random component
  u <- mvrnorm(n = periods + 2*h + 1, 
               mu = rep(0, K),
               Sigma = sigma)
  
  #Creating matrix of values
  y <- matrix(0,
              nrow = periods + 2*h + 1, #periods + h blanks + h+1 "burn-ins"
              ncol = K)

  #Adding values 
  for (t in (h+1):nrow(y)) { #h+1 leaves the h blank rows
    y[t, ] <- c(1, y[(t-h):(t-1), ]) %*% beta + u[t, ]
  }
  
  #Deleting blanks and "burn-ins"
  y <- y[-(1:(2*h + 1)), ]
  u <- u[-(1:h), ]
  
  #Creating output list 
  output <- list("K" = K,
                 "h" = h,
                 "periods" = periods,
                 "beta" = beta,
                 "sigma" = sigma,
                 "u" = u,
                 "y" = y)
  
  #Returning output 
  return(output)
  
}
