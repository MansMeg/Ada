#' @title
#' rinvgamma
#' 
#' @param n See n in rgamma()
#' @param shape Alpha 
#' @param scale Beta 
#' 
#' @details
#' Samples an inverse gamma (taken from MCMCpack)
rinvgamma <- function (n, shape, scale = 1) {
  return(1/rgamma(n = n, shape = shape, rate = scale))
}



#' @title
#' sampleTau2
#' 
#' @details
#' Samples tau^2, the house design effect in the model 
#' 
#' @param prior A 2 * h matrix containing alpha and beta as columns and a prior for each house
#' @param Yt Observation matrix (rows are observations and columns are parties)
#' @param bt State matrix (rows are observations and columns are parties)
#' 
#' @return 
#' A matrix with each matrix as a row
#' 
sampleTau2 <- function(prior, Yt, bt){
  # Assertions
  stopifnot(is.data.frame(Yt) & is.data.frame(bt))
  stopifnot(all(c(colnames(bt), "house", "n_eff") %in% colnames(Yt)))
  stopifnot(all(dim(Yt) >= dim(bt)))
  stopifnot(dim(prior)[2] == 2 & nrow(prior) <= length(levels(Yt$house))) 
  stopifnot(all(rownames(prior)%in%levels(Yt$house)))
  
  # Data converting
  Yt$time <- as.character(Yt$time)
  
  # Split
  L <- ncol(bt)
  h <- Yt$house
  H <- length(levels(h))
  Yt <- merge(x=Yt,y=bt,by.x="time",by.y="row.names",all=TRUE,sort=FALSE)
  Yt <- Yt[,c(str_join(colnames(bt),".x"), str_join(colnames(bt),".y"), "n_eff")]
  dataList <- split(Yt,h)
  
  #  dat <- dataList[[1]]
  Th <- lapply(dataList,FUN=nrow)
  
  addBeta <-  unlist(lapply(dataList,FUN=.calcPostPerHouse, L=L)) / 2
  addAlpha <-  L * unlist(Th) / 2
  
  postSample <-  suppressWarnings(rinvgamma(L, prior[,"alpha"] + addAlpha, prior[,"beta"] + addBeta))
  names(postSample) <- rownames(prior)
  postSample[is.nan(postSample)] <- 1 # Set priorfree tau to 1 (we know the variance)
  return(postSample)
}



#' @title
#' .calcPostPerHouse
#' 
#' @details
#' Function to calculate (yt-bt)^T*Sigma^-1(yt-bt) in sampleTau2
#' 
#' @param dat
#' data.frame with Y-values (column 1 to L) estimated state (column L + 1 to 2*L) and the number of observations (column 2*L + 1)
#' @param L
#' the number of parties
#' 
.calcPostPerHouse <- function(dat,L){
  eachrow <- apply(dat,1, 
                   FUN=function(X){
                     diff <- as.matrix(X[1:L]-X[(L+1):(2*L)])
                     mat <- .createInvCovMatrix(p=X[1:L],n=X[(2*L + 1)])
                     res <- t(diff)%*%mat%*%diff
                     return(res)
                   })
  return(sum(eachrow))
}



#' @title
#' sampleMissingYnorm
#' 
#' @details
#' Samples missing values of Y. Assuming a MVN distribution (with Covar as multinomial)
#' 
#' @param statesMissing Matrix with missing states to sample from
#' @param samplesize The assumed sample size to sample from as a normal distribution
#' 
#' @return 
#' A matrix where values for Y has been sampled.
#' 
#' 
sampleMissingYnorm <- function(statesMissing, samplesize=500){
  # Assertions
  
  # Create matrix to store results
  rsums <- rep(2,dim(statesMissing)[1])
  it <- 10
  sampleY <- statesMissing
  
  # Rejection sampling
  while(!all(rsums <= 1) & it > 1){
    sampleY[rsums > 1,] <-   
      t(apply(X=statesMissing[rsums > 1,,drop=FALSE], 1, FUN= function(X) {
        mvrnorm(1,mu=X,Sigma=.createCovMatrix(X, samplesize),)    
      }))
    rsums <- rowSums(sampleY)
    it <- it - 1
  }
  
  # If rejection sampling fails totally resort to simply normalizing
  if(it==1){
    sampleY[rsums > 1,] <- sampleY[rsums > 1,] / rsums[rsums > 1]
  }
  
  return(sampleY)
}



#' @title
#' sampleMissingY
#' 
#' @details
#' Samples missing values of Y. Assuming a dirichlet distribution
#' 
#' @param statesMissing Matrix with missing states to sample from
#' @param samplesize The assumed sample size to sample from as a normal distribution
#' 
#' @return 
#' A matrix where values for Y has been sampled.
#' 
sampleMissingY <- function(statesMissing, samplesize=500){
  # Assertions and requires
  # require(bayesm)
  stopifnot(all(!is.na(statesMissing)))
  
  statesMissing <- .correctStates(states=statesMissing, pMin=0.001)
  other <- 1 - rowSums(statesMissing)
  
  statesMissing <-   
    t(apply(X=cbind(statesMissing, other), 1,
            FUN= function(X) bayesm::rdirichlet(X*samplesize))    
    )
  
  return(statesMissing[,-ncol(statesMissing)])
}

#' @title
#' sampleTau2period
#' 
#' @details
#' Samples tau^2, the house design effect in the model, 
#' but it compares the poll estimates with the period of the poll, not just the time
#' 
#' @param prior A 2 * h matrix containing alpha and beta as columns and a prior for each house
#' @param Yt Observation matrix (rows are observations and columns are parties)
#' @param bt State matrix (rows are observations and columns are parties)
#' 
#' @return 
#' A matrix with each matrix as a row
#' 
sampleTau2period <- function(prior, Yt, bt){
  # Assertions
  stopifnot(is.data.frame(Yt) & is.data.frame(bt))
  stopifnot(all(c(colnames(bt), "house", "n_eff", "periodFrom", "periodTo") %in% colnames(Yt)))
  stopifnot(all(dim(Yt) >= dim(bt)))
  stopifnot(dim(prior)[2] == 2 & nrow(prior) <= length(levels(Yt$house))) 
  stopifnot(all(rownames(prior)%in%levels(Yt$house)))
  stopifnot(min(Yt$periodFrom) >= min(ymd(dimnames(bt)[[1]])))
  
  
  # Split
  L <- ncol(bt)
  h <- Yt$house
  H <- length(levels(h))
  
  # Data converting
  Yt$time <- as.character(Yt$time)
  Yt$period <- paste(as.character(Yt$periodFrom),as.character(Yt$periodTo),sep=" - ")
  Yt <- Yt[!duplicated(Yt[,c("house", "period")]),]
  periodLength <- as.numeric(Yt$periodTo - Yt$periodFrom) + 1
  Yt$n <- Yt$n * periodLength # Recalculate n
  Yt$ind <- 1:nrow(Yt)
  
  betaValues <- unlist(lapply(split(Yt, f=Yt$ind), .calcPeriodMean, bt=bt))
  addBeta <- tapply(X=betaValues, INDEX=Yt$house, FUN=sum, na.rm=TRUE)
  
  Th <- as.vector(table(Yt$house)[rownames(prior)])
  addAlpha <-  L * Th / 2
  
  postSample <-  suppressWarnings(rinvgamma(H, prior[,"alpha"] + addAlpha, addBeta + prior[,"beta"]))
  names(postSample) <- rownames(prior)
  postSample[is.nan(postSample)] <- 1 # Set prior free tau to 1 (we know the variance)
  stopifnot(length(postSample) == nrow(prior))
  return(postSample)
}

#' @title
#' .calcPeriodMean
#' 
#' @details
#' Function to calculate (yt-bt)^T*Sigma^-1(yt-bt) in sampleTau2period.
#' 
#' @param X
#' Row from Yt to calculate over
#' @param bt
#' states matrix to use for calculations
#' 
.calcPeriodMean <- function(X, bt){
  partyNames <- dimnames(bt)[[2]]
  # Assertions
  stopifnot(is.data.frame(X))
  stopifnot(nrow(X)==1)
  stopifnot(all(partyNames %in% colnames(X)))
  stopifnot(c("periodFrom", "periodTo", "time", "n") %in% colnames(X))
  
  # Do calculations
  period <- as.numeric(X$periodTo - X$periodFrom)
  periodDays <- as.character(X$periodFrom + days(0:period))
  btMean <- colMeans(bt[periodDays,,drop=FALSE])[partyNames]
  invSigma <- .createInvCovMatrix(p=X[,partyNames], n=X[,"n"])
  btMinusYt <- matrix(btMean - as.numeric(X[,partyNames]),ncol=1)
  res <- t(btMinusYt) %*% invSigma %*% btMinusYt
  return(res)
}

