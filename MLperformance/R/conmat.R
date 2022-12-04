##' Creates a confusion matrix.
##' 
##' Uses the table generated from cmcounts() function, or a vector of binary outcomes to create a labelled confusion matrix
##' @title Confusion Matrix 
##' @param counts=table (cmcounts() table) or vector containing frequencies of actual vs. predicted binary variables :  c(00,01,10,11)
##' @return Labelled confusion matrix
##' @author Barbara Breeden
##' @export
##' @examples 
##' 
##' counts <- c(43,20,5,31) #frequencies of binary outcomes: 00,01,10,11
##' cm <- conmat(counts)
##' cm
##' 
##' counts <- cmcounts(data=barretts)
##' cm <- conmat(counts)
##' cm
##' 

conmat <- function(counts){
  conmatrix <- matrix(c(counts[4], counts[3], counts[2], counts[1]), ncol = 2, byrow = TRUE)
  colnames(conmatrix) <- c("Predicted Pos (PP)", "Predicted Neg (PN)")
  rownames(conmatrix) <- c("Actual Pos (P)", "Actual Neg (N)")
  conmatrix
}