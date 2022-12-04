##' Computes the metrics for the confusion matrix
##' 
##' (P)-Total positive, (N)-Total negative, (PP)-Predicted positive, (PN)-Predicted negative, 
##' (TP)-True positive, (TN)-True negative, (FP)- False positive, (FN)-False negative, (n)-sample size
##' @title Confusion Matrix Metrics
##' @param cm=matrix :confusion matrix object from conmat() function.
##' @return dataframe of metrics variables: P,N,PP,PN,TP,TN,FP,FN,n 
##' 
##' @author Barbara Breeden
##' @export
##' @examples 
##' 
##' counts <- cmcounts(barretts)
##' cm <- conmat(counts)
##' met <- metrics(cm)
##' met
##' 

metrics <- function(cm){
  
  P <- sum(cm[1,]) #total actual positives
  N <- sum(cm[2,]) #total actual negatives
  PP <- sum(cm[,1]) #total predicted positives
  PN <- sum(cm[,2]) #total predicted negatives
  TP <- cm[1,1] #true positives
  TN <- cm[2,2] #true negatives
  FP <- cm[2,1] #false positives
  FN <- cm[1,2] #false negatives
  n <- TP+TN+FP+FN #sample size
  result <- list(P,N,PP,PN,TP,TN,FP,FN,n)
  names(result) <- c("P","N","PP","PN","TP","TN","FP","FN","n")
  result <- as.data.frame(result)
  result
}