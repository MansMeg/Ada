#' @title
#' .createCovMatrix
#' 
#' @details
#' Create a covariance matrix approximation for a dirichlet distribution
#' 
#' @param p proportions vector of size L
#' @param n number of observations
#' 
#' @return 
#' A matrix of size L*L
#' 
.createCovMatrix <- function(p, n){
  # Creates a covariance matrix (V) for a measurement that is assumed as a multivariate normal approximation of the multinomial
  stopifnot(all(p > 0) & sum(p) <= 1)
  # Give name of p to matrix
  p <- as.numeric(p); n <- as.numeric(n)
  # Old solution with multinomial covariance
#   covMat <- -(as.matrix(p)%*%t(as.matrix(p)))/n
#   covMat[diag(length(p))==1] <- p*(1-p)/n
#   return(covMat)
  return(diag((p*(1-p)/n))) # New, assume independence

}


#' @title
#' .createInvCovMatrix
#' @details
#' Create a inverse covariance matrix approximation for a dirichlet distribution
#' 
#' @param p proportions vector of size L
#' @param n number of observations
#' 
#' @return 
#' A matrix of size L*L
#' 
.createInvCovMatrix <- function(p,n){
  # Creates a covariance matrix (V) for a measurement that is assumed as a multivariate normal approximation of the multinomial
  stopifnot(all(p > 0) & sum(p) <= 1)

  p <- as.numeric(p); n <- as.numeric(n)
  # Old solution: singular multinomial matrix
#  covMat <- -(as.matrix(p)%*%t(as.matrix(p)))/n
#  covMat[diag(length(p))==1] <- p*(1-p)/n
#  return(solve(covMat))
  return(diag(1/(p*(1-p)/n))) # New, assume independence
}


#' @title
#' imputeStartValues
#' 
#' @details
#' Impute starting values for missing observations with linear approximation between observations
#' 
#' @param dataset Dataset to use to impute values
#' @param varsToImpute Which variables should be imputed (variable names)
#' @param pred
#' The prior predicition/true result of the election
#' 
#' @return 
#' A dataset where values for varsToImpute has been imputed.
#' 
#' 
imputeStartValues <- function(dataset, varsToImpute, pred){
  # Assertions
  stopifnot(c(varsToImpute, "house") %in% colnames(dataset))
  stopifnot(all(varsToImpute == names(pred)))
  stopifnot(dataset$time[1]<=dataset$time[2])

  lastRow <- dataset[1,]
  lastRow[,varsToImpute] <- pred
  lastRow[,"house"] <- levels(dataset$house)[1]
  dataset <- rbind(dataset, lastRow)
  
  nonmissing <- which(!is.na(dataset[,"house"]))
  for(i in 1:(length(nonmissing)-1)){
    if(nonmissing[i+1]!=nonmissing[i]+1){
      js <- (nonmissing[i]+1):(nonmissing[i+1]-1)
      b <- (dataset[nonmissing[i+1],varsToImpute] - dataset[nonmissing[i],varsToImpute]) / 
        (length(js)+1)
      for(j in seq_along(js)){
        dataset[js[j], varsToImpute] <- dataset[nonmissing[i], varsToImpute]  + b*j
      }
    }
  }
  return(dataset[-nrow(dataset),])
}



#' @title
#' .correctStates
#' 
#' @details
#' Takes a state vector matrix and correct it to all p > 0 and sum(p) <= pMin
#' 
#' @param states The states to correct
#' @param pMin the minimal value of 'other'
#' 
#' @return 
#' A matrix where values for Y has been sampled.
#' 
.correctStates <- function(states, pMin){
  # Assertions
  
  # Calc
  above_pMin <- states > pMin
  if(any(!above_pMin)) states[!above_pMin] <- pMin
  rsums <- rowSums(states)
  states[rsums > (1 - pMin), ] <- states[rsums > (1 - pMin),] / (rsums[rsums > (1 - pMin)] + pMin)
  
  return(states)
}


#' @title
#' timeInvX
#' 
#' @details
#' Calculates the time variant V matrix
#' 
#' @param pMatrix Matrix with proportions as rows
#' @param n vector vith no of observations
#' 
#' @return 
#' A matrix with each matrix as a row
#' 

timeInvX <- function(pMatrix, n){
  # Assertions
  stopifnot(all(rowSums(pMatrix) < 1 | is.na(rowSums(pMatrix))))
  stopifnot(all(pMatrix > 0 | is.na(pMatrix)))
  stopifnot(all(n > 0 | is.na(n)))

  # Bookkeeping
  row.names(pMatrix) <- NULL
  Xstart <- cbind(pMatrix,n)
  isNotNA <- !apply(is.na(Xstart),1,all)
  # V for missing data
  missV <- as.vector(diag(ncol(pMatrix)))  
  X <- matrix(missV,nrow=nrow(pMatrix), ncol=ncol(pMatrix)^2, byrow=TRUE)
  X[isNotNA,] <-   
    t(apply(X=Xstart[isNotNA,], 1,
            FUN = createVectorInTimeInvX))  
  return(X)
}

#' @title
#' createVectorInTimeInvX
#' 
#' @details
#' Internal function used in timeInvX
#' 
#' @param X A row from Xstart in timeInvX
#' 
createVectorInTimeInvX <- function(X) {
  # Assertions
  stopifnot(all(!is.na(X)))
  
  return(as.vector(.createCovMatrix(p=X[-length(X)], n=X[length(X)])))
}

#' @title
#' weightPolls
#' 
#' @details
#' Weights all polls conducted the same day by the effective sample size
#' 
#' @param Ydata Dataframe containing n_eff, time, the partynames ordered in decreasing order
#' @param partyNames vector with partynames
#' 
#' @return 
#' A matrix with each matrix as a row
#' 
weightPolls <- function(Ydata, partyNames){
  # Assertions 
  stopifnot(c("time", partyNames, "n_eff") %in% names(Ydata))
  stopifnot(class(Ydata)=="data.frame")
  stopifnot(Ydata$time[1] > Ydata$time[length(Ydata$time)])
  
  Ydata$Other <- 1 - rowSums(Ydata[,partyNames])
  splitDF <- split(x=Ydata[,c(partyNames, "Other", "n_eff")], f=Ydata$time)
  l <- lapply(X=splitDF, FUN=.calcWeightPollNP, partyNames=partyNames)
  res <- as.data.frame(rbindlist(l))
  colnames(res) <- c(partyNames, "n_eff")
  res$time <- ymd(names(l))
  res <- res[order(res$time,decreasing=TRUE),]
  return(res[,c("time", partyNames, "n_eff")])
}

.calcWeightPollNP <- function(X, partyNames){
  X <- as.matrix(X)
  np <- t(X[, "n_eff"]) %*% X[,c(partyNames, "Other")]
  sumnp <- sum(np)
  return(as.data.frame(matrix(c(np[-length(np)]/sumnp,sumnp),nrow=1)))
}



#' @title
#' .calcQuantilesTime
#' 
#' @details
#' Calculates the quantiles for a bt in an ElectPredict object
#' 
#' @param obj An ElectPredict object
#' @param intervals The probability intervals to be calculated
#' 
#' @return
#' list with quantiles per date and party
#' 
.calcQuantilesTime <- function(obj, intervals=c(0.9, 0.75)){
  # Assertions
  stopifnot(class(obj) == "ElectPred")
  
  # Calculate quantiles
  quant <- 0.5
  for (j in intervals){
    quant <- c(quant, (1 - j) / 2,  1 - (1 - j) / 2)
  }
  quant <- quant[order(quant)]
  
  quantList <- list()
  for (i in dimnames(obj$bt)[[2]]){
    quantList[[i]] <- t(apply(X = obj$bt[,i,],MARGIN=1,quantile, probs = quant))
  }
  
  return(quantList)
}


#' @title
#' expandPollToPeriod
#' 
#' @details
#' Expand a poll to one observation each day. Missing houses are removed.
#' 
#' @param
#' polls Dataframe with (at least) variables periodFrom, periodTo, house, time, n
#' 
#' @return
#' Expanded dataset with one poll per day
#' 
expandPollToPeriod <- function(polls=Y_all){
  # Assertions
  stopifnot(c("periodFrom", "periodTo", "house", "time", "n") %in% colnames(polls))
  
  polls <- polls[!is.na(polls$house),]
  polls$ind <- 1:nrow(polls)
  polldays <- split(x=polls,f=polls$ind)  
  expPolls <- rbindlist(lapply(X=polldays, FUN=.expandPoll))
  expPolls <- expPolls[order(expPolls$time, decreasing=TRUE),]
  expPolls$ind <- NULL
  return(expPolls)
}

#' @title
#' .expandPoll
#' 
#' @details
#' Internal function used in expandPollToPeriod
#' 
#' @param
#' X one row that are to be expanded.
#' 
#' @return
#' Expanded dataset with one poll per day
#' 
.expandPoll <- function(X){
  # Assertions
  stopifnot(is.data.frame(X))
  stopifnot(nrow(X)==1)
  stopifnot(c("periodFrom", "periodTo", "time", "n") %in% colnames(X))
  
  period <- as.numeric(X$periodTo - X$periodFrom)
  X <- X[rep(1,period+1),] 
  X$time <- X$periodFrom[1] + days(0:period)
  X$n <- X$n/(period+1)
  
  return(X)  
}


