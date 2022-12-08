## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## ---- eval=FALSE--------------------------------------------------------------
#  ParatoGen <- function(n){
#    f_inv <- function(u) 2/(1-u)^0.5
#    x <- f_inv(runif(n))
#  }

## -----------------------------------------------------------------------------
library(StatComp22087)
x <- ParatoGen(1e4)
hist(x, freq=FALSE, breaks=300, xlim = c(0,50))
f <- function(x) 8*x^{-3}
x <- seq(0,50,0.5)
lines(x, f(x))

## ---- eval=FALSE--------------------------------------------------------------
#  BetaGen <- function(n, a=1, b=1){
#    x0 <- (a-1)/(a+b-2)
#    c <- x0^(a-1)*(1-x0)^(b-1)
#    i <- 0
#    s <- numeric(n)
#    repeat{
#      x <- runif(1)
#      u <- runif(1)
#      if (u <= x^(a-1)*(1-x)^(b-1)/c){
#        i <- i+1
#        s[i] <- x
#      }
#      if(i==n) break
#    }
#    return(s)
#  }

## -----------------------------------------------------------------------------
x <- BetaGen(1000, a=3, b=2)
hist(x, freq = FALSE, breaks = 15)
B0 <- function(x) x^2*(1-x)^1
z <- integrate(B0, 0, 1)$value
B <- function(x) B0(x)/z
lines(seq(0,1,0.01), B(seq(0,1,0.01)))

## ---- eval=FALSE--------------------------------------------------------------
#  MixGen <- function(n, gamma=4, beta=2){
#    lambda <- rgamma(n, gamma, beta)
#    y <- rexp(n, lambda)
#  }

## -----------------------------------------------------------------------------
gamma=4
beta=2
y <- MixGen(1e4)
hist(y,  freq=FALSE, breaks=30)
x <- seq(0,12,0.1)
f <- function(x) gamma*beta^gamma*(beta+x)^(-gamma-1)
lines(x, f(x))

## -----------------------------------------------------------------------------
quick_sort<-function(x){
  num<-length(x)
  if(num==0||num==1){return(x)
  }else{
    a<-x[1]
    y<-x[-1]
    lower<-y[y<a]
    upper<-y[y>=a]
    return(c(quick_sort(lower),a,quick_sort(upper)))}
}

## -----------------------------------------------------------------------------
Caltime <- function(n, num_repeat=100){
  t <- numeric(num_repeat)
  for (i in 1:num_repeat){
    s <- sample(1:n)
    t[i] <- system.time(quick_sort(s))[1]
  }
  return(mean(t))
}
# apply Caltime to n
n <- c(1e4, 2e4, 4e4, 6e4, 8e4)
a <- unlist(lapply(n, Caltime))
# regression and plot
t <- n*log(n)
d = as.data.frame(cbind(t,a))
r = lm(a~t, d)
plot(d, main = "Regress 'a' on 't'")
abline(r)

## ---- eval=FALSE--------------------------------------------------------------
#  MonteGen <- function(n, antithetic=TRUE){
#    u <- runif(n/2)
#    if(antithetic) v <- 1-u
#    else v <- runif(n/2)
#    u <- c(u,v)
#    x <- mean(exp(u))
#    return(x)
#  }

## -----------------------------------------------------------------------------
n <- 1e4
m <- 1e4
sMC <- aMC <- numeric(m)
for(i in 1:m){
  sMC[i] <- MonteGen(n, antithetic = FALSE)
  aMC[i] <- MonteGen(n)
} 
(var(sMC)-var(aMC))/var(sMC)

## -----------------------------------------------------------------------------
x <- seq(0, 10, 0.01)
g <- function(x){
  x^2*exp(-x^2/2)/sqrt(2*3.14159)
}
plot(x, g(x)/(2*dnorm(x,1)), type = 'l', ylim = c(0,1), main = "comparing the ratio", ylab = "ratios of g and the candidate functions")
lines(x, g(x)/dgamma(x-1, 2, 1.8), col="red")

## ---- eval=FALSE--------------------------------------------------------------
#  StrImpSam <- function(n, m=100){
#    thetai <- function(i, n){
#      u <- runif(n)
#      x <- i/5+log(1/(1-(1-exp(-1/5))*u)) #inverse distribution
#      thetaihat <- mean(exp(-i/5)*(1-exp(-1/5))/(1+x^2))
#      return(thetaihat)
#    }
#    theta <- numeric(m)
#    for (j in 1:m) theta[j] <- sum(vapply(0:4,thetai,1))
#  }

## -----------------------------------------------------------------------------
cat("Stratified:", var(StrImpSam(n=100)))
cat("\nNaive:",var(ImpSam(n=500)))

## ---- eval=FALSE--------------------------------------------------------------
#  CI_lognormal <- function(x){
#    y <- log(x)
#    yhat <- mean(y)
#    yse <- sd(y)
#    m <- length(x)
#    lc <- yhat + yse/sqrt(m)*qt(c(0.025, 0.975), df=m-1)
#  }

## -----------------------------------------------------------------------------
m <- 50
n <- 1e3
count <- 0
for(i in 1:n){
  x <- rlnorm(m)
  lc <- CI_lognormal(x)
  if (lc[1]<=0&lc[2]>0) count <- count+1
}
count/n

## -----------------------------------------------------------------------------
count5test <- function(x, y){
  X <- x - mean(x)
  Y <- y - mean(y)
  outx <- sum(X > max(Y)) + sum(X < min(Y))
  outy <- sum(Y > max(X)) + sum(Y < min(X))
  return(as.integer(max(c(outx, outy)) > 5))
}
Ftest <- function(x, y){
  as.integer(var.test(x,y)$p.value<=0.055)
}

## -----------------------------------------------------------------------------
tests <- function(x, y){
  c5t <- count5test(x,y)
  ft <- Ftest(x,y)
  c(c5t, ft)
}
sd1 <- 1
sd2 <- 1.5
for(k in 1:3){
  power <- matrix(numeric(2000), nrow = 1000)
  for(i in 1:1000){
    n <- 2*10^k
    x <- rnorm(n, sd=sd1)
    y <- rnorm(n, sd=sd2)
    power[i,] <- tests(x,y)
  }
  cat(2*10^k, colMeans(power), "\n")
}

## -----------------------------------------------------------------------------
t <- c(3,5,7,18,43,85,91,98,100,130,230,487)
cat("The MLE of lambda is", 1/mean(t))

## -----------------------------------------------------------------------------
B <- 1e4
mB <- numeric(B)
for(i in 1:B){
  x <- sample(t, replace = T)
  mB[i] <- 1/mean(x)
}
cat("The bias of the estimate is", mean(mB)-1/mean(t))
cat("The standard error of the estimate is", sd(mB))

## -----------------------------------------------------------------------------
library(boot)
mymean <- function(x,i) mean(x[i])
de <- boot(data=t, statistic=mymean, R=999)
ci <- boot.ci(de, type=c("norm","basic","perc","bca"))
ci.norm <- ci$norm[2:3]
ci.basic <- ci$basic[4:5]
ci.perc <- ci$percent[4:5]
ci.bca <- ci$bca[4:5]
cat("The normal interval is", ci.norm[1], ci.norm[2])
cat("The basic interval is", ci.basic[1], ci.basic[2])
cat("The percentile interval is", ci.perc[1], ci.perc[2])
cat("The BCa interval is", ci.bca[1], ci.bca[2])

## -----------------------------------------------------------------------------
sam_ge <- function(n, mu, sigma){
  x <- rnorm(n, mu, sigma)
}
sam_an <- function(n, mu, sigma, R, m){
  mymean <- function(x, i) mean(x[i])
  L <- C <- numeric(3)
  for(i in 1:m){
    x <- sam_ge(n, mu, sigma)
    de <- boot(data=x, statistic=mymean, R=R)
    ci <- boot.ci(de, type=c("norm","basic","perc"))
    ci.norm <- ci$norm[2:3]
    ci.basic <- ci$basic[4:5]
    ci.perc <- ci$percent[4:5]
    if(ci.norm[1]<=mu & ci.norm[2]>=mu) C[1] <- C[1]+1
    else if(ci.norm[1]>mu) L[1] <- L[1]+1
    if(ci.basic[1]<=mu & ci.basic[2]>=mu) C[2] <- C[2]+1
    else if(ci.basic[1]>mu) L[2] <- L[2]+1
    if(ci.perc[1]<=mu & ci.perc[2]>=mu) C[3] <- C[3]+1
    else if(ci.perc[1]>mu) L[3] <- L[3]+1
  }
  return(list(C, L))
}
sam_res <- function(n, mu=0, sigma=1, R, m){
  res <- sam_an(n, mu, sigma, R, m)
  rate <- res[[1]]/m
  pro_L <- res[[2]]/m
  pro_R <- 1-rate-pro_L
  pander::pander(data.frame("method"=c("normal","basic","percentile"), "coverage rate"=rate, "left miss"=pro_L, "right miss"=pro_R))
}

## ---- echo=F------------------------------------------------------------------
print("n=10, R=100, m=1000")
sam_res(n=10, R=100, m=1000)
print("n=1000, R=100, m=1000")
sam_res(n=1000, R=100, m=1000)

## -----------------------------------------------------------------------------
library(bootstrap)
n <- nrow(scor)
theta.jack <- numeric(n)
lambda <- eigen(cov(scor))$value
theta.hat <- lambda[1]/sum(lambda)
for(i in 1:n){
  lambda <- eigen(cov(scor[-i,]))$value
  theta.jack[i] <- lambda[1]/sum(lambda)
}
bias <- (n-1)*(mean(theta.jack)-theta.hat)
variance <- var(theta.jack)*(n-1)^2/n
sd <- sqrt(variance)
cat("The bias is:", bias)
cat("The standard error is:", sd)

## -----------------------------------------------------------------------------
library(DAAG)
attach(ironslag)
n <- length(magnetic)  
N <- n*(n-1)/2 # all possible combination
e1 <- e2 <- e3 <- e4 <- numeric(n)
h<-1
# leave-two-out cross validation
for (i in 1:(n-1)) {
  for (j in (i+1):n) {
    k<-c(i,j)
    y <- magnetic[-k]
    x <- chemical[-k]
    # Model 1: Linear
    J1 <- lm(y ~ x)
    yhat1 <- J1$coef[1] + J1$coef[2]*chemical[k]
    e1[h] <- sum((magnetic[k] - yhat1)^2)
    # Model 2：Quadratic
    J2 <- lm(y ~ x + I(x^2))
    yhat2 <- J2$coef[1] + J2$coef[2]*chemical[k] +J2$coef[3]*chemical[k]^2
    e2[h] <- sum((magnetic[k] - yhat2)^2)
    # Model 3: Exponential
    J3 <- lm(log(y) ~ x)
    logyhat3 <- J3$coef[1] + J3$coef[2]*chemical[k]
    yhat3 <- exp(logyhat3)
    e3[h] <- sum((magnetic[k] - yhat3)^2)
    # Model 4: Log-Log
    J4 <- lm(log(y) ~ log(x))
    logyhat4 <- J4$coef[1] + J4$coef[2] * log(chemical[k])
    yhat4 <- exp(logyhat4)
    e4[h] <- sum((magnetic[k] - yhat4)^2)
    h<-h+1
  }
}
# the average squared prediction error by leave-two-out cross validation
c(Linear=sum(e1), Quadratic=sum(e2), Exponential=sum(e3), LogLog=sum(e4))/(2*N)

## -----------------------------------------------------------------------------
library(boot)
library(MASS)
spearman <- function(z){
  rho <- function(data, i){
    x <- data[,1]
    y <- data[i,2]
    rho <- cor(x, y, method = "spearman")# in bivariate case, it equals to Spearman's rank correlation coefficient
    return(rho)
  }
  rhos <- boot(data=z, statistic=rho, R=9999, sim="permutation")$t
  rho0 <- cor(z[,1], z[,2], method = "spearman")
  p <- (sum(rhos>=rho0)+1)/10000
  return(p)
}
z1 <- mvrnorm(1000, c(1,2), matrix(c(1,0.001,0.001,1),2,2)) #almost independent samples
cat("cor.test pvalue:", cor.test(z1[,1], z1[,2], method = "spearman")$p.value)
cat("our spearman test pvalue:", spearman(z1))
z2 <- mvrnorm(1000, c(1,2), matrix(c(1,0.1,0.1,1),2,2))# dependent samples
cat("cor.test pvalue:", cor.test(z2[,1], z2[,2], method = "spearman")$p.value)
cat("our spearman test pvalue:", spearman(z2))

## -----------------------------------------------------------------------------
#chain generation
MCMC <- function(x0, g, f, n, sigma){
  x <- rep(0, n)
  x[1] <- x0
  k <- 0
  for(i in 2:n){
    xt <- x[i] <- x[i-1]
    y <- rnorm(1, xt, sigma)
    r <- f(y)*g(xt, y, sigma)/(f(xt)*g(y, xt, sigma))
    u <- runif(1)
    if (u<=r){
      x[i] <- y
      k <- k+1
    } 
  }
  return(list(chain=x, acc=k/n))
}
#Gelman-Rubin
Gelman.Rubin <- function(psi){
  psi <- as.matrix(psi) #psi is the cumulative-mean-value matrix 
  n <- ncol(psi)
  k <- nrow(psi)
  psi.means <- rowMeans(psi)
  B <- n*var(psi.means)
  psi.w <- apply(psi, 1, var)
  W <- mean(psi.w)
  v.hat <- W*(n-1)/n+B/(n*k)
  r.hat <- v.hat/W
  return(r.hat)
}

## -----------------------------------------------------------------------------
#Generate the samples
f <- function(x){
  1/2*exp(-abs(x))
}
g <- function(x, xt, sigma){
  dnorm(x, xt, sigma)
}
X <- list()
accept <- list()
for (i in 1:5){
  x <- sapply(c(0,5,10), MCMC, g, f, 15000, 0.1*10^(i-1))
  X[[i]] <- rbind(x[1,1]$chain, x[1,2]$chain, x[1,3]$chain)
  accept[[i]] <- c(x[2,1]$acc, x[2,2]$acc, x[2,3]$acc)
}
#The acceptance rate
acceptance_rate <- data.frame("0.1"=accept[[1]],
                              "1"=accept[[2]],
                              "10"=accept[[3]],
                              "100"=accept[[4]],
                              "1000"=accept[[5]])
rownames(acceptance_rate) <- c("x0=0", "x0=5", "x0=10")
colnames(acceptance_rate) <- c("sigma=0.1", "sigma=1", "sigma=10", "sigma=100", "sigma=1000")
pander::pander(acceptance_rate)

## -----------------------------------------------------------------------------
R <- numeric(5)
for(j in 1:5){
  psi <- t(apply(X[[j]], 1, cumsum))
  for(i in 1:nrow(psi)) psi[i,] <- psi[i,]/1:ncol(psi)
  R[j] <- Gelman.Rubin(psi)
}
pander::pander(data.frame("sigma0.1"=R[1], "sigma1"=R[2], "sigma10"=R[3], "sigma100"=R[4], "sigma1000"=R[5]))

## -----------------------------------------------------------------------------
for(i in 1:5){
  par(mfrow=c(1,3))
  for (j in 1:nrow(X[[i]])) plot(X[[i]][j,], type="l", xlab=0.1*10^(i-1), ylab="value")
  par(mfrow=c(1,1))
}

## -----------------------------------------------------------------------------
#Gibbs generation function
Gibbs <- function(x0, n){
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
#Generate samples
X0 <- matrix(c(0,0,1,2,7,8), byrow = T, nrow = 3)
sams <- list()
for(i in 1:3) sams[[i]] <- Gibbs(X0[i,], 15000)
#Calculate R_x and R_y
x <- rbind(sams[[1]][,1], sams[[2]][,1], sams[[3]][,1])
psi_x <- t(apply(x, 1, cumsum))
for(j in 1:nrow(psi_x)) psi_x[j,] <- psi_x[j,]/1:ncol(psi_x)
y <- rbind(sams[[1]][,2], sams[[2]][,2], sams[[3]][,2])
psi_y <- t(apply(y, 1, cumsum))
for(j in 1:nrow(psi_y)) psi_y[j,] <- psi_y[j,]/1:ncol(psi_y)
pander::pander(data.frame("x"=Gelman.Rubin(psi_x), "y"=Gelman.Rubin(psi_y), row.names = "R"))
#Plot
for(i in 1:nrow(x)){
  par(mfrow=c(1,2))
  plot(x[i,5000:15000], type="l", ylab="x_trace", xlab=x[i,1])
  plot(y[i,5000:15000], type="l", ylab="y_trace", xlab=y[i,1])
  par(mfrow=c(1,1))
}
#We use chain1, that is, x0=(0,0) chain to do regression
y1 <-  y[1,5000:15000]
x1 <- x[1,5000:15000]
plot(x1, y1)
(c1 <- lm(y1~x1))
plot(c1)

## -----------------------------------------------------------------------------
generation <- function(condition, am, ay, n, gamma=1){
  if(condition==1){
    x <- rnorm(n)
    ey <- rnorm(n)
    m <- rnorm(n, am, 1)
    y <- ay + gamma*x + ey 
  }
  if(condition==2){
    x <- rnorm(n)
    ey <- rnorm(n)
    m <- rnorm(n, am, 1)
    y <- ay + gamma*x + ey + m 
  }
  if(condition==3){
    x <- rnorm(n)
    ey <- rnorm(n)
    m <- rnorm(n, am, 1) + x
    y <- ay + gamma*x + ey 
  }
  return(list(x=x, y=y, m=m))
}
simulation <- function(condition, x, y, m, B){
  # full model
  fit1 <- lm(m~x)
  fit2 <- lm(y~m+x)
  # reduced model
  if(condition==1){
    fit3 <- lm(m~1)
    fit4 <- lm(y~m+x)
  }
  if(condition==2){
    fit3 <- lm(m~x)
    fit4 <- lm(y~x)
  }
  if(condition==3){
    fit3 <- lm(m~1)
    fit4 <- lm(y~x)
  }
  # permutation
  mhat <- fit3$fitted.values
  yhat <- fit4$fitted.values
  rm <- fit3$residuals
  ry <- fit4$residuals
  n <- length(rm)
  alpha <- beta <- numeric(B)
  for(i in 1:B){
    lab <- sample(1:(2*n))
    r <- c(rm, ry)[lab]
    rmstar <- r[1:n]
    rystar <- r[-(1:n)]
    mstar <- mhat + rmstar
    ystar <- yhat + rystar
    fit3 <- lm(mstar~x)
    fit4 <- lm(ystar~m+x)
    alpha[i] <- fit3$coefficients[2]
    beta[i] <- fit4$coefficients[2]
  }
  alpha0 <- fit1$coefficients[2]
  beta0 <- fit2$coefficients[2]
  t <- alpha*beta
  t0 <- alpha0*beta0
  p <- (sum(abs(t)>=t0)+1)/(B+1)
  return(p)
}

## -----------------------------------------------------------------------------
# parameters condition 1
data <- generation(1, 1, 1, 100)
x <- data$x
y <- data$y
m <- data$m
# permutation condition 1
p11 <- simulation(1, x, y, m, 500)
# permutation condition 2
p12 <- simulation(2, x, y, m, 500)
# permutation condition 3
p13 <- simulation(3, x, y, m, 500)
# parameters condition 2
data <- generation(2, 1, 1, 100)
x <- data$x
y <- data$y
m <- data$m
# permutation condition 1
p21 <- simulation(1, x, y, m, 500)
# permutation condition 2
p22 <- simulation(2, x, y, m, 500)
# permutation condition 3
p23 <- simulation(3, x, y, m, 500)
# parameters condition 3
data <- generation(3, 1, 1, 100)
x <- data$x
y <- data$y
m <- data$m
# permutation condition 1
p31 <- simulation(1, x, y, m, 500)
# permutation condition 2
p32 <- simulation(2, x, y, m, 500)
# permutation condition 3
p33 <- simulation(3, x, y, m, 500)

## -----------------------------------------------------------------------------
p1 <- c(p11, p12, p13)
p2 <- c(p21, p22, p23)
p3 <- c(p31, p32, p33)
p <- rbind(p1, p2, p3)
p <- as.data.frame(p)
colnames(p) <- c("H1", "H2", "H3")
rownames(p) <- c("D1", "D2", "D3")
pander::pander(p)

## -----------------------------------------------------------------------------
# the function
Falpha <- function(f0, N, b1, b2, b3){
  x1 <- rpois(N, 1)
  x2 <- rexp(N, 1)
  x3 <- rbinom(N, 1, 0.5)
  g <- function(alpha){
    tmp <- exp(-alpha-b1*x1-b2*x2-b3*x3)
    p <- 1/(1+tmp)
    mean(p)-f0
  }
  solution <- uniroot(g, c(-100,10))
  solution$root
}

## -----------------------------------------------------------------------------
f0 <- c(1e-1, 1e-2, 1e-3, 1e-4)
alpha <- vapply(f0, Falpha, 1, N=1e6, b1=0, b2=1, b3=-1)
plot(f0, alpha)
plot(-log(f0), alpha)

## -----------------------------------------------------------------------------
MLE <- function(lambda, u, v){
  tab <- (-u*exp(-lambda*u)+v*exp(-lambda*v))/(exp(-lambda*u)-exp(-lambda*v))
  sum(tab)
}
u <- c(11,8,27,13,16,0,23,10,24,2)
v <- u+1
(lambda <- uniroot(MLE, c(1e-5, 10), u=u, v=v)$root)

## -----------------------------------------------------------------------------
EM <- function(lambda0, u, v){
  tab <- 1
  k <- 0
  n <- length(u)
  while(tab>1e-6&k<=1000){
    lambda1 <- n/(n/lambda0 - sum((-u*exp(-lambda0*u)+v*exp(-lambda0*v))/(exp(-lambda0*u)-exp(-lambda0*v))))
    tab <- abs(lambda0-lambda1)
    lambda0 <- lambda1
  }
  return(lambda1)
}
EM(1, u, v)

## -----------------------------------------------------------------------------
a <- list(a=c(1,2,3), b=4, c="c")
b <- unlist(a)
cat("If b is a vector:", is.vector(b))
cat("The length of b:", length(b))
print(b) 
c <- as.vector(a)
cat("If c is a vector:", is.vector(c))
cat("The length of c:", length(c), "\n")
print(c)

## -----------------------------------------------------------------------------
a <- c(1,2)
dim(a)

## -----------------------------------------------------------------------------
a <- matrix(1:4, nrow = 2)
is.array(a)

## -----------------------------------------------------------------------------
attributes(data.frame(a))

## -----------------------------------------------------------------------------
a <- data.frame(name <- c("a", "b"), age <- c(1,3))
as.matrix(a)

## -----------------------------------------------------------------------------
a <- data.frame(matrix(nrow=0, ncol=0))
str(a)
b <- data.frame(matrix(nrow=0, ncol=1))
str(b)
c <- data.frame(matrix(nrow=1, ncol=0))
str(c)

## -----------------------------------------------------------------------------
scale01 <- function(x) {
  rng <- range(x, na.rm = TRUE)
  (x - rng[1]) / (rng[2] - rng[1])
}

## -----------------------------------------------------------------------------
X <- as.data.frame(iris3)
Y <- apply(X, 2, function(x) if(is.numeric(x)) scale01(x) else x)
head(X)
head(Y)

## -----------------------------------------------------------------------------
(stn <- vapply(X, sd, 1))
(stm <- vapply(iris[vapply(iris, is.numeric, logical(1))],  sd, 1))

## -----------------------------------------------------------------------------
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

## -----------------------------------------------------------------------------
library(Rcpp)
library(microbenchmark)
ts <- microbenchmark(gibbsR=GibbsR(c(0,0),1000),
                     gibbsC=GibbsC(0,0,1000))
summary(ts)[,c(1,3,5,6)]
gibbsR <- GibbsR(c(0,0),1000)[500:1000,]
gibbsC <- GibbsC(0,0,1000)[500:1000,]
qqplot(gibbsR[,1], gibbsC[,1])
abline(0, 1, lwd=1.5, col="red")
qqplot(gibbsR[,2], gibbsC[,2])
abline(0, 1, lwd=1.5, col="red")

