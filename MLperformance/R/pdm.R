##' Computes the prevalence dependent metrics from the confusion matrix
##' 
##' Prevalence, (PPV)- positive predictive value, (NPV)- negative predictive value, (F1)- F1 score, (FDR)- false discovery rate,
##' (FOR)- false omission rate, (MCC), Matthews correlation coefficient
##' 
##' @title Prevalence Dependent Metrics
##' @param cm=matrix :confusion matrix object from conmat() function.
##' @return dataframe of metrics variables: Prevalence, PPV, NPV, F1, FDR, FOR, MCC
##' 
##' @author Barbara Breeden
##' @export
##' @examples 
##' 
##' counts <- cmcounts(barretts)
##' cm <- conmat(counts)
##' pdm <- pdm(cm)
##' pdm
##' 


pdm <- function(cm){
  P <- sum(cm[1,]) #total actual positives
  N <- sum(cm[2,]) #total actual negatives
  PP <- sum(cm[,1]) #total predicted positives
  PN <- sum(cm[,2]) #total predicted negatives
  TP <- cm[1,1] #true positives
  TN <- cm[2,2] #true negatives
  FP <- cm[2,1] #false positives
  FN <- cm[1,2] #false negatives
  n <- TP+TN+FP+FN #sample size
  TPR <- TP/(TP+FN)
  TNR <- TN/(FP+TN)
  FPR <- 1 - TNR
  FNR <- 1-TPR
  prevalence <- P/n
  PPV <- TP/(TP+FP)
  NPV <- TN/(FN+TN) 
  F1 <- (2*PPV*TPR)/(PPV+TPR)
  FDR <- 1-PPV 
  FOR <- 1-NPV 
  MCC <- sqrt(PPV*TPR*TNR*NPV)-sqrt(FDR*FOR*FNR*FPR) #Matthews Correlation Coefficient
  result <- list(prevalence,PPV,NPV,F1,FDR,FOR,MCC)
  names(result) <- c("prevalence","PPV","NPV","F1","FDR","FOR","MCC")
  result <- as.data.frame(result)
  result
}