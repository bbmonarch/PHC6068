##' Computes the prevalence independent metrics from the confusion matrix
##' 
##' (ACC)-Accuracy, (TPR)- True Positive rate, "Sensitivity", (TNR)- True Negative Rate, "Specificity", (BA)- Balanced Accuracy,
##' (BM)- Bookmaker informedness, (FPR)- False Positive Rate, (FNR)- False Negative Rate, (PLR)- Positive Likelihood Ratio, 
##' (NLR)- Negative Likelihood Ratio, (DOR)- Diagnostic Odds Ratio
##' 
##' @title Prevalence Independent Metrics
##' @param cm=matrix :confusion matrix object from conmat() function.
##' @return dataframe of metrics variables: ACC, TPR, TNR, BA, BM, FPR, FNR, PLR, NLR, DOR
##' 
##' @author Barbara Breeden
##' @export
##' @examples 
##' 
##' counts <- cmcounts(barretts)
##' cm <- conmat(counts)
##' pim <- pim(cm)
##' pim
##' 


pim <- function(cm){
  P <- sum(cm[1,]) #total actual positives
  N <- sum(cm[2,]) #total actual negatives
  PP <- sum(cm[,1]) #total predicted positives
  PN <- sum(cm[,2]) #total predicted negatives
  TP <- cm[1,1] #true positives
  TN <- cm[2,2] #true negatives
  FP <- cm[2,1] #false positives
  FN <- cm[1,2] #false negatives
  n <- TP+TN+FP+FN #sample size
  ACC <- (TP+TN)/n
  TPR <- TP/(TP+FN)
  TNR <- TN/(FP+TN)
  BA <- (TPR+TNR)/2
  BM <- TPR+TNR-1
  FPR <- 1 - TNR
  FNR <- 1-TPR
  PLR <- TPR/FPR
  NLR <- FNR/TNR
  DOR <- PLR/NLR
  result <- list(ACC, TPR, TNR, BA, BM, FPR, FNR, PLR, NLR, DOR)
  names(result) <- c("ACC","TPR","TNR","BA","BM","FPR","FNR","PLR","NLR","DOR")
  result <- as.data.frame(result)
  result
}
