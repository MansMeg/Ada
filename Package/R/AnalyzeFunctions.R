
#' @title
#' ExpectPred
#' 
#' @details
#' Analyze function to calculate the expected prediction at the election.
#' 
#' @param obj An "ElectPred" object.
#' 
#' @return
#' Returns a vector with the expected prediction.
#'
#' @export
#' 
ExpectPred <- function(x, day=NULL){
  if(is.null(day)) day <- as.character(x$param$electionDay)  
  stopifnot(class(x) == "ElectPred")
  res <- rowMeans(x$bt[day,,])
  res <- c(res, 1-sum(res))
  names(res)[length(res)] <- "Other"
  return(res)
}


#' @title
#' ExpectCovariance
#' 
#' @details
#' Expected state Covariance Matrix
#' 
#' @param x An "ElectPred" object.
#' 
#' @return
#' Returns a vector with the expected prediction.
#'
#' @export
#' 
ExpectCovariance <- function(x){
  stopifnot(class(x) == "ElectPred")
  Wb <- x$Wb[,,1] 
  for(i in 2:dim(x$Wb)[3]){
    Wb <- Wb + x$Wb[,,i]
  }
  Wb <- Wb / dim(x$Wb)[3]
  return(Wb)
}

#' @title
#' ExpectStDev
#' 
#' @details
#' Expected state Covariance Matrix
#' 
#' @param x An "ElectPred" object.
#' 
#' @return
#' Returns a vector with the expected prediction.
#'
#' @export
#' 
ExpectStDev <- function(x){
  StDev <- sqrt(diag(x$Wb[,,1]))
  for(i in 2:dim(x$Wb)[3]){
    StDev <- StDev + sqrt(diag(x$Wb[,,i]))
  }
  StDev <- StDev / dim(x$Wb)[3]
  return(StDev)
}



#' @title
#' ExpectCorr
#' 
#' @details
#' Expected state Covariance Matrix
#' 
#' @param x An "ElectPred" object.
#' 
#' @return
#' Returns a vector with the expected prediction.
#'
#' @export
#' 
ExpectCorr <- function(x){
  corrMat <- cov2cor(x$Wb[,,1])
  for(i in 2:dim(x$Wb)[3]){
    corrMat <- corrMat + cov2cor(x$Wb[,,i])
  }
  corrMat <- corrMat / dim(x$Wb)[3]
  return(corrMat)
}


#' @title
#' ProbNorm
#' 
#' @details
#' Normalizing the probability at electionDay (other parties always counts as 0).
#' 
#' @param x An "ElectPred" object.
#' @param lowLimit Lower limit, parties below are set to 0. If NULL lower limit is 0.
#' @param normalize 
#' Should the results be normalized (recalculate based on removed parties). If not, "other" is added.
#' 
#' @return
#' Returns a dataframe with samples
#'
#' @export
#' 
ElectionProb <- function(x, lowLimit=NULL, normalize=TRUE, day=NULL){
  if(is.null(day)) day <- as.character(x$param$electionDay)    
  stopifnot(class(x) == "ElectPred")
  dat <- as.data.frame(t(x$bt[day,,]))
  if(!is.null(lowLimit)){
    for(j in 1:ncol(dat)){
      dat[dat[,j] < lowLimit, j] <- 0
    }
    if(normalize) dat <- dat / rowSums(dat)
  }
  if(!normalize) dat$other <- 1 - rowSums(dat)
  return(dat)
}


