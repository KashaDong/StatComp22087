#' @title PC1: data from NASA dataset
#' @name PC1
#' @description A dataset used to illustrate the performance of \code{MAtree}.
#' @examples
#' \dontrun{
#' data(PC1)
#' MAtree(PC1)
#' }
NULL

#' @title AEEEM: experiment results from AEEEM
#' @name AEEEM
#' @description A dataset used to illustrate the performance of \code{MAtree}.
#' @examples
#' \dontrun{
#' data(AEEEM_exp)
#' pander(AEEEM)
#' }
#' @importFrom pander pander
NULL

#' @title NASA: experiment results from NASA
#' @name NASA
#' @description A dataset used to illustrate the performance of \code{MAtree}.
#' @examples
#' \dontrun{
#' data(AEEEM_exp)
#' pander(AEEEM)
#' }
#' @importFrom pander pander
NULL

#' @title Generate an index vector to split the data-set
#' @description \code{Ksplit} is a function, which can generate an index vector to split the data-set to \code{K} parts.
#' @param K int, the number of folds divided.
#' @param n int, the number of samples.
#' @return a vector of splitting index.
#' @examples
#' \dontrun{
#' K <- 5
#' n <- 17
#' Ksplit(K, n)
#' }
#' @export
Ksplit <- function(K, n){
  J <- ceiling(n/K)
  loc <- rep(1:K, J)[1:n]
  loc <- sample(loc)
  return(loc)
}

#' @title Evaluate the performance of the prediction
#' @description \code{perform} is a function that can evaluate the performance of the prediction.
#' @param y_true vector, the true response.
#' @param y_pred vector, the prediction of the response.
#' @param y_proba vector, the predicted probability of the response.
#' @return a list of performance with \code{Accuracy}, \code{Precision}, \code{Recall}, \code{F1}, \code{AUC}.
#' @examples
#' \dontrun{
#' data(PC1)
#' datafull <- dataloader(1, loc, PC1)
#' X_test <- as.matrix(datafull$X_test)
#' y_test <- as.matrix(datafull$y_test)
#' X_train <- as.matrix(datafull$X_train)
#' y_train <- as.matrix(datafull$y_train)
#' dtrain = lgb.Dataset(X_train, label = y_train)
#' model <- lightgbm(data=dtrain, verbose=-1, params = list(objective="binary"))
#' yr <- predict(model, X_test)
#' yp <- c(0,1)[(yr>0.2)+1]
#' str(perform(y_test, yp, yr))
#' }
#' @import pROC
#' @export
perform <- function(y_true, y_pred, y_proba){
  TP <- FP <- FN <- TN <- 0
  n <- length(y_true)
  y <- y_true
  p <- y_pred
  pr <- y_proba
  for(i in 1:n){
    if(p[i]==y[i] & y[i]==1) TP=TP+1
    if(p[i]==y[i] & y[i]==0) TN=TN+1
    if(p[i]!=y[i] & y[i]==0) FP=FP+1
    if(p[i]!=y[i] & y[i]==1) FN=FN+1
  }
  ACC = (TP+TN)/(TP+FN+FP+TN)
  Precision = TP/(TP+FP)
  Recall = TP/(TP+FN)
  F1 = (2*Precision*Recall)/(Precision+Recall)
  AUC = auc(response = as.vector(y), predictor = as.vector(pr), levels = c(0, 1), direction = "<")
  result <- list(Accuracy=ACC, Precision=Precision, Recall=Recall, F1=F1, AUC=AUC)
  return(result)
}

#' @title prepare the kth fold data
#' @description \code{dataloader} is a function that prepares the kth training data and testing data
#' @param k int, the kth fold.
#' @param loc vector, the index of splitting.
#' @param data matrix, the data to be divided.
#' @return a list of \code{X_train}, \code{y_train}, \code{X_test}, \code{y_test}
#' @examples
#' \dontrun{
#' data(PC1)
#' n <- nrow(PC1)
#' K <- 5
#' loc <- Ksplit(K, n)
#' dim(dataloader(1, loc, PC1))
#' dim(PC1)
#' }
#' @export
dataloader <- function(k, loc, data){
  m <- ncol(data)
  X_test <- as.matrix(data[loc==k,-m])
  y_test <- as.matrix(data[loc==k,m])
  X_train <- as.matrix(data[loc!=k,-m])
  y_train <- as.matrix(data[loc!=k,m])
  result <- list(X_test=X_test, y_test=y_test, X_train=X_train, y_train=y_train)
  return(result)
}

#' @title Calculate the weight of model averaging
#' @description \code{Kfold_weight} is a function that calculates the weight of model averaging.
#' @param data the data used to calculate weight.
#' @param K K-fold cross validation model averaging method.
#' @param c the threshold to be positive.
#' @param method optimization methods: "squareloss" only.
#' @return a vector of weight.
#' @examples
#' \dontrun{
#' data(PC1)
#' Kfold_weight(PC1, 5, 0.2)
#' }
#' @importFrom lightgbm lightgbm lgb.Dataset
#' @importFrom xgboost xgboost xgb.DMatrix
#' @importFrom quadprog solve.QP
#' @importFrom stats predict
#' @useDynLib StatComp22087
#' @export
Kfold_weight <- function(data, K, c, method="squareloss"){
  n <- nrow(data)
  index <- Ksplit(K,n)
  prediction <- probability <- array(dim = c(0,2))
  true <-  array(dim = c(0,1))
  for(i in 1:K){
    datafull <- dataloader(i, index, data)
    X_test <- datafull$X_test
    y_test <- datafull$y_test
    X_train <- datafull$X_train
    y_train <- datafull$y_train
    # gbm
    gbm_train = lgb.Dataset(X_train, label=y_train)
    # xgb
    xgb_train <- xgb.DMatrix(data=X_train, label=y_train)
    # prediction
    xgb_model <- xgboost(data = xgb_train, params = list(objective="binary:logistic"), verbose = -1, nrounds=10)
    gbm_model <- lightgbm(data = gbm_train, params = list(objective="binary"), verbose = -1)
    xgb_pr <- predict(xgb_model, X_test)
    gbm_pr <- predict(gbm_model, X_test)
    xgb_p <- c(0,1)[(xgb_pr>c)+1]
    gbm_p <- c(0,1)[(gbm_pr>c)+1]
    prediction <- rbind(prediction, cbind(gbm_p, xgb_p))
    probability <- rbind(probability, cbind(gbm_pr, xgb_pr))
    true <- rbind(true, y_test)
  }
  if(method=="squareloss"){
    e <- prediction-cbind(true,true)
    D <- 2*t(e)%*%e
    d <- c(0,0)
    A <- rbind(diag(c(1,1)), diag(c(-1,-1)))
    A <- rbind(A, c(1,1))
    b <- c(0,0,-1,-1,1)
    w <- solve.QP(Dmat=D, dvec=d, Amat=t(A), bvec=b)$solution
  }
  return(w)
}

#' @title Model averaging on \code{xgboost} and \code{lightgbm}
#' @description Conduct model averaging method on \code{xgboost} and \code{lightgbm} to do software defect prediction.
#' @param data the data used to train and test.
#' @param k k-fold cross validation to evaluate the performance.
#' @param m m=fold cross validation model averaging method.
#' @param c the threshold to be positive.
#' @param method optimization methods: "squareloss" only.
#' @return The prediction and the performance of \code{lightgbm}, \code{xgboost} and \code{MAtree}.
#' @examples
#' \dontrun{
#' data(PC1)
#' str(MAtree(PC1))
#' }
#' @export
MAtree <- function(data, k=5, m=3, c=0.2, method="squareloss"){
  n <- nrow(data)
  p <- ncol(data)
  loc <- Ksplit(k, n)
  prediction <- probability <- array(dim = c(0,3))
  true <-  array(dim = c(0,1))
  for(i in 1:k){
    datafull <- dataloader(i, loc, data)
    train <- cbind(datafull$X_train, datafull$y_train)
    test <- cbind(datafull$X_test, datafull$y_test)
    w <- Kfold_weight(train, K=m, c=c, method)
    # pre and combine
    X_test <- datafull$X_test
    y_test <- datafull$y_test
    X_train <- datafull$X_train
    y_train <- datafull$y_train
    # gbm
    gbm_train = lgb.Dataset(X_train, label = y_train)
    # xgb
    xgb_train <- xgb.DMatrix(data = X_train, label = y_train)
    # prediction
    xgb_model <- xgboost(data = xgb_train, verbose = -1, params = list(objective="binary:logistic"), nrounds = 30)
    gbm_model <- lightgbm(data = gbm_train,  params = list(objective="binary"), verbose = -1)
    xgb_pr <- predict(xgb_model, X_test)
    gbm_pr <- predict(gbm_model, X_test)
    MA_pr <- w[1]*gbm_pr+w[2]*xgb_pr
    xgb_p <- c(0,1)[(xgb_pr>c)+1]
    gbm_p <- c(0,1)[(gbm_pr>c)+1]
    MA_p <- c(0,1)[(MA_pr>c)+1]
    prediction <- rbind(prediction, cbind(gbm_p, xgb_p, MA_p))
    probability <- rbind(probability, cbind(gbm_pr, xgb_pr, MA_pr))
    true <- rbind(true, y_test)
  }
  #gbm
  gbm_perform <- perform(true, prediction[,1],probability[,1])
  xgb_perform <- perform(true, prediction[,2],probability[,2])
  MA_perform <- perform(true, prediction[,3],probability[,3])
  return(list(prediction=prediction, gbm=gbm_perform, xgb=xgb_perform, MA=MA_perform))
}