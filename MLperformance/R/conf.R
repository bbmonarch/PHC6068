##' Computes the 95% confidence interval for diagnostic odds ratio.
##' 
##' 
##' @title Diagnostic odds ratio 95% confidence interval
##' @param cm=matrix :confusion matrix object from conmat() function.
##' @return Diagnostic odds ratio 95% confidence interval
##' 
##' @author Barbara Breeden
##' @export
##' @examples 
##' 
##' counts <- cmcounts(barretts)
##' cm <- conmat(counts)
##' conf <- conf(cm)
##' conf
##' 


conf <- function(cm){
  P <- sum(cm[1,]) #total actual positives
  N <- sum(cm[2,]) #total actual negatives
  PP <- sum(cm[,1]) #total predicted positives
  PN <- sum(cm[,2]) #total predicted negatives
  TP <- cm[1,1] #true positives
  TN <- cm[2,2] #true negatives
  FP <- cm[2,1] #false positives
  FN <- cm[1,2] #false negatives
  TPR <- TP/(TP+FN)
  TNR <- TN/(FP+TN)
  FPR <- 1 - TNR
  FNR <- 1-TPR
  PLR <- TPR/FPR
  NLR <- FNR/TNR
  DOR <- PLR/NLR
  LowerCI <- exp((log(DOR))-(1.96*(sqrt((1/TP)+(1/TN)+(1/FP)+(1/FN)))))
  UpperCI <- exp((log(DOR))+(1.96*(sqrt((1/TP)+(1/TN)+(1/FP)+(1/FN)))))
  paste0("[",round(LowerCI,2),",",round(UpperCI,2),"]")
}