##' Counts the frequencies for confusion matrix cells: 00,01,10,11.
##' 
##' Load a dataframe with two binary variables:  column 1 = "actual" condition and column 2 = "predicted" condition
##' @title Confusion Matrix Counts
##' @param data=dataframe with two binary variables
##' @return counts of 00,01,10,11
##' @author Barbara Breeden
##' @export
##' @examples 
##' 
##' cmcounts(data=barretts)
##' 



cmcounts <- function(data) {
    
    colnames(data) <- c("actual", "predicted")
    data$predicted <- factor(data$predicted)
    data$actual <- factor(data$actual)
    table <- table(paste(data$actual, data$predicted, sep = ""))
    print(table)
}
