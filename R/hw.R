#' @title Generate \code{Parato(2,2)} samples by inverse transformation method
#' @description Function \code{ParatoGen} implements inverse transformation method to generate `Parato(2,2)` samples of size `n`.
#' @param n int, the number of samples.
#' @return a vector of samples of length \code{n}.
#' @examples
#' \dontrun{
#' ParatoGen(1e4)
#' }
#' @importFrom stats runif
#' @export
ParatoGen <- function(n){
  f_inv <- function(u) 2/(1-u)^0.5
  x <- f_inv(runif(n))
}

#' @title Generate \code{Beta(a,b)} samples by AR method
#' @description Function `BetaGen` implements Acceptance-Rejection method to generate $Beta(a,b)$ samples of size `n`.
#' @param n int, the number of samples.
#' @param a float, first parameter of beta distribution.
#' @param b float, second parameter of beta distribution.
#' @return a vector of samples of length \code{n}.
#' @examples
#' \dontrun{
#' BetaGen(1e4)
#' }
#' @export
BetaGen <- function(n, a=1, b=1){
  x0 <- (a-1)/(a+b-2)
  c <- x0^(a-1)*(1-x0)^(b-1)
  i <- 0
  s <- numeric(n)
  repeat{
    x <- runif(1)
    u <- runif(1)
    if (u <= x^(a-1)*(1-x)^(b-1)/c){
      i <- i+1
      s[i] <- x
    }
    if(i==n) break
  }
  return(s)
}

#' @title Generate Exponential-Gamma mixture samples
#' @description Function `MixGen` simulates a continuous Exponential-Gamma mixture.
#' @param n int, the number of samples.
#' @param gamma float, first parameter of gamma distribution.
#' @param beta float, second parameter of gamma distribution.
#' @return a vector of samples of length \code{n}.
#' @examples
#' \dontrun{
#' MixGen(1e4)
#' }
#' @importFrom stats rgamma rexp
#' @export
MixGen <- function(n, gamma=4, beta=2){
  lambda <- rgamma(n, gamma, beta)
  y <- rexp(n, lambda)
}

#' @title Compare simple Monte Carlo and antithetic variate approach
#' @description Function `MonteGen` can calculate integration of $e^x$ on $[0,1]$ with or without antithetic variate approach.
#' @param n int, the number of times to do Monte Carlo simulation.
#' @param antithetic bool, use antithetic or not.
#' @return Monte Catlo simulation result.
#' @examples
#' \dontrun{
#' MonteGen(100)
#' }
#' @export
MonteGen <- function(n, antithetic=TRUE){
  u <- runif(n/2)
  if(antithetic) v <- 1-u
  else v <- runif(n/2)
  u <- c(u,v)
  x <- mean(exp(u))
  return(x)
}

#' @title Implement stratified importance sampling method
#' @description Function `StrImpSam` implements the stratified importance sampling method.
#' @param n int, the number of times to do Monte Carlo.
#' @param m int, the number of times to estimate.
#' @return a vector of estimation of length \code{m}.
#' @examples
#' \dontrun{
#' StrImpSam(300)
#' }
#' @export
StrImpSam <- function(n, m=100){
  thetai <- function(i){
    u <- runif(n)
    x <- i/5+log(1/(1-(1-exp(-1/5))*u)) #inverse distribution
    thetaihat <- mean(exp(-i/5)*(1-exp(-1/5))/(1+x^2))
    return(thetaihat)
  }
  theta <- numeric(m)
  for (j in 1:m) theta[j] <- sum(vapply(0:4,thetai,1))
  return(theta)
}

#' @title Implement importance sampling method
#' @description Function `ImpSam` implements the importance sampling method.
#' @param n int, the number of times to do Monte Carlo.
#' @param m int, the number of times to estimate.
#' @return a vector of estimation of length \code{m}.
#' @examples
#' \dontrun{
#' ImpSam(300)
#' }
#' @export
ImpSam <- function(n, m=100){
  theta <- numeric(m)
  for(i in 1:m){
    u <- runif(n)
    x <- -log(1-(1-exp(-1))*u)
    theta[i] <- mean((1-exp(-1))/(1+x^2))
  }
  return(theta)
}

#' @title Monte Carlo method to obtain empirical confidence interval
#' @description Function `CI_lognormal` implements the Monte Carlo method to obtain empirical confidence interval.
#' @param x vector, a sample of length `n`.
#' @return a confidence interval simulated by Monte Carlo method.
#' @examples
#' \dontrun{
#' x <- rlnorm(50)
#' CI_lognormal(x)
#' }
#' @export
#' @importFrom stats sd qt
CI_lognormal <- function(x){
  y <- log(x)
  yhat <- mean(y)
  yse <- sd(y)
  m <- length(x)
  lc <- yhat + yse/sqrt(m)*qt(c(0.025, 0.975), df=m-1)
}

#' @title Implement Gibbs sampler by R
#' @description Function `GibbsR` implements the Gibbs sampler by R.
#' @param x0 vector of length 2, a start value.
#' @param n int, the number of the sample to be generated.
#' @return a sample of length n.
#' @examples
#' \dontrun{
#' GibbsR(c(0,0), 1000)
#' }
#' @importFrom stats rnorm
#' @export
GibbsR <- function(x0, n){
  p <- length(x0)
  sam <- matrix(numeric(p*n), nrow = n)
  sam[1,] <- x0
  sig <- sqrt(0.19)
  for(i in 2:n){
    sam[i,1] <- rnorm(1, 0.9*sam[i-1,2], sig)
    sam[i,2] <- rnorm(1, 0.9*sam[i,1], sig)
  }
  return(sam)
}

#' @title Some packages needed in vignettes
#' @name packages
#' @import Rcpp boot bootstrap DAAG microbenchmark
#' @importFrom stats rlnorm
NULL
