## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## ---- eval=FALSE--------------------------------------------------------------
#  Ksplit <- function(K, n){
#    J <- ceiling(n/K)
#    loc <- rep(1:K, J)[1:n]
#    loc <- sample(loc)
#    return(loc)
#  }

## ---- eval=FALSE--------------------------------------------------------------
#  dataloader <- function(k, loc, data){
#    m <- ncol(data)
#    X_test <- as.matrix(data[loc==k,-m])
#    y_test <- as.matrix(data[loc==k,m])
#    X_train <- as.matrix(data[loc!=k,-m])
#    y_train <- as.matrix(data[loc!=k,m])
#    result <- list(X_test=X_test, y_test=y_test, X_train=X_train, y_train=y_train)
#    return(result)
#  }

## ---- eval=FALSE--------------------------------------------------------------
#  perform <- function(y_true, y_pred, y_proba){
#    TP <- FP <- FN <- TN <- 0
#    n <- length(y_true)
#    y <- y_true
#    p <- y_pred
#    pr <- y_proba
#    for(i in 1:n){
#      if(p[i]==y[i] & y[i]==1) TP=TP+1
#      if(p[i]==y[i] & y[i]==0) TN=TN+1
#      if(p[i]!=y[i] & y[i]==0) FP=FP+1
#      if(p[i]!=y[i] & y[i]==1) FN=FN+1
#    }
#    ACC = (TP+TN)/(TP+FN+FP+TN)
#    Precision = TP/(TP+FP)
#    Recall = TP/(TP+FN)
#    F1 = (2*Precision*Recall)/(Precision+Recall)
#    AUC = auc(response = as.vector(y), predictor = as.vector(pr))
#    result <- list(Accuracy=ACC, Precision=Precision, Recall=Recall, F1=F1, AUC=AUC)
#    return(result)
#  }

## ---- eval=FALSE--------------------------------------------------------------
#  Kfold_weight <- function(data, K, c, method="squareloss"){
#    n <- nrow(data)
#    index <- Ksplit(K,n)
#    prediction <- probability <- array(dim = c(0,2))
#    true <-  array(dim = c(0,1))
#    for(i in 1:K){
#      datafull <- dataloader(i, index, data)
#      X_test <- datafull$X_test
#      y_test <- datafull$y_test
#      X_train <- datafull$X_train
#      y_train <- datafull$y_train
#      # gbm
#      gbm_train = lgb.Dataset(X_train, label=y_train)
#      # xgb
#      xgb_train <- xgb.DMatrix(data=X_train, label=y_train)
#      # prediction
#      xgb_model <- xgboost(data = xgb_train, verbose = -1, params = list(objective="binary:logistic"), nrounds=10)
#      gbm_model <- lightgbm(data = gbm_train, verbose = -1, params = list(objective="binary"))
#      xgb_pr <- predict(xgb_model, X_test)
#      gbm_pr <- predict(gbm_model, X_test)
#      xgb_p <- c(0,1)[(xgb_pr>c)+1]
#      gbm_p <- c(0,1)[(gbm_pr>c)+1]
#      prediction <- rbind(prediction, cbind(gbm_p, xgb_p))
#      probability <- rbind(probability, cbind(gbm_pr, xgb_pr))
#      true <- rbind(true, y_test)
#    }
#    if(method=="squareloss"){
#      e <- prediction-cbind(true,true)
#      D <- 2*t(e)%*%e
#      print(D)
#      d <- c(0,0)
#      A <- rbind(diag(c(1,1)), diag(c(-1,-1)))
#      A <- rbind(A, c(1,1))
#      b <- c(0,0,-1,-1,1)
#      w <- solve.QP(Dmat=D, dvec=d, Amat=t(A), bvec=b)$solution
#    }
#    return(w)
#  }

## ---- eval=FALSE--------------------------------------------------------------
#  MAtree <- function(data, k=5, m=3, c=0.2, method="squareloss"){
#    n <- nrow(data)
#    p <- ncol(data)
#    loc <- Ksplit(k, n)
#    prediction <- probability <- array(dim = c(0,3))
#    true <-  array(dim = c(0,1))
#    for(i in 1:k){
#      datafull <- dataloader(i, loc, data)
#      train <- cbind(datafull$X_train, datafull$y_train)
#      test <- cbind(datafull$X_test, datafull$y_test)
#      w <- Kfold_weight(train, K=m, c=c, method)
#      cat("weight",  w)
#      # pre and combine
#      X_test <- datafull$X_test
#      y_test <- datafull$y_test
#      X_train <- datafull$X_train
#      y_train <- datafull$y_train
#      # gbm
#      gbm_train = lgb.Dataset(X_train, label = y_train)
#      # xgb
#      xgb_train <- xgb.DMatrix(data = X_train, label = y_train)
#      # prediction
#      xgb_model <- xgboost(data = xgb_train, verbose = 0, params = list(objective="binary:logistic"), nrounds = 30)
#      gbm_model <- lightgbm(data = gbm_train, verbose = -1, params = list(objective="binary"))
#      xgb_pr <- predict(xgb_model, X_test)
#      gbm_pr <- predict(gbm_model, X_test)
#      MA_pr <- w[1]*gbm_pr+w[2]*xgb_pr
#      xgb_p <- c(0,1)[(xgb_pr>c)+1]
#      gbm_p <- c(0,1)[(gbm_pr>c)+1]
#      MA_p <- c(0,1)[(MA_pr>c)+1]
#      prediction <- rbind(prediction, cbind(gbm_p, xgb_p, MA_p))
#      probability <- rbind(probability, cbind(gbm_pr, xgb_pr, MA_pr))
#      true <- rbind(true, y_test)
#    }
#    #gbm
#    gbm_perform <- perform(true, prediction[,1],probability[,1])
#    xgb_perform <- perform(true, prediction[,2],probability[,2])
#    MA_perform <- perform(true, prediction[,3],probability[,3])
#    return(list(prediction=prediction, gbm=gbm_perform, xgb=xgb_perform, MA=MA_perform))
#  }

## ---- results='hide'----------------------------------------------------------
library(StatComp22087)
data(PC1)
result <- MAtree(PC1)

## -----------------------------------------------------------------------------
print(result$gbm)
print(result$xgb)
print(result$MA)

## -----------------------------------------------------------------------------
data("AEEEM_exp")
data("NASA_exp")
pander::pander(AEEEM)
pander::pander(NASA)

