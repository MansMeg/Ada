#' @title
#' print.ElectPred
#' 
#' @param x An ElectPredict object
#' 
#' @return
#' Print parameters
#'
#' @export
print.ElectPred <- function(x){
  stopifnot(class(x) == "ElectPred")
  cat("ElectPred class object:\n")
  cat("Election (at ", as.character(x$param$electionDay), 
      ") predicted at ", as.character(x$param$predictionDay), ".\n", sep="")
  cat("A total of ",nrow(x$data)," polls from ", length(unique(as.character(x$data$house))), 
      " houses (starting at ",as.character(min(x$data$periodFrom)),").\n", sep="")
  cat("MCMC: ",x$param$MCMCsamples," samples with a burnin of ",x$param$burnin,
      " and a thinning of ", x$param$thin,".\n", sep="")
  cat("Total runtime: ~ ",round(as.numeric(x$runtime, units = "mins"))," minutes.\n",sep="")  
}


#' @title
#' summary
#' 
#' @details
#' Summation of interest results.
#' 
#' @param x An "ElectPred" object.
#' 
#' @return
#' Returns a vector with the expected prediction.
#'
#' @export
#' 
summary.ElectPred <- function(x, lowLimit=0.04, day=NULL, alpha=0.05){
  print(x)
  cat("\n\nExpected results:\n")
  expected <- as.matrix(ExpectPred(x, day=day))
  colnames(expected) <- "Expected results"
  print(expected)
  
  cat("\n\nProbability Intervals (",(1-alpha)*100,"%):\n",sep="")
  probsResult <- ElectionProb(x=x, lowLimit=0, normalize=FALSE, day=day)
  print(t(apply(probsResult, 2, quantile, probs=c(alpha/2, .5, 1 - alpha/2))))
    
  probs <- ElectionProb(x=x, lowLimit=lowLimit)
  cat("\n\nParliment results probability (with limit set to ",lowLimit,"):\n", sep="")
  print(t(apply(probs, 2, quantile, probs=c(alpha/2, 0.5, 1 - alpha/2))))
  
  return(invisible("PrintOut"))
}

