#' @title
#' electPredict
#' 
#' @details
#' Creates a prediction for a multiparty system.
#' 
#' @param priors A list of prior specifications for the model.
#' @param parameters Day of election, day of prediction
#' @param data a dataset with prespecified time variable and house variable. See details.
#' 
#' @details
#' the dataset should contain each poll as a row. The main variables needed is \code{time} 
#' (date of the poll - needs to be in POSIX format), \code{n} the numer of observation in the poll,
#' and \code{house}, a factor variable of polling house.
#' 
#' @return 
#' An electionPredict object.
#' 
#' @export

electPredict <-function(priors, parameters, data){
  ## Require
  require(dlm)
  require(data.table)
  require(lubridate)
  require(stringr)
  require(bayesm)
  
  # Create time stamp
  timeStamp <- Sys.time()
  
  ## Assertions
  # Parameters
  stopifnot(c("electionDay", "timeInterval", 
              "burnin", "thin", "MCMCsamples", "seed", "silent", 
              "timeVarName","publicationDateVarName","sampleSizeVarName",
              "houseVarName","periodVarNames", "partyVarNames") %in% names(parameters))
  stopifnot(is.null(parameters$periodVarNames) | parameters$periodVarNames %in% names(data))
  stopifnot((is.numeric(parameters$timeInterval) & parameters$timeInterval >= 1))
  stopifnot("POSIXt" %in% class(parameters$electionDay))
  stopifnot("POSIXt" %in% class(parameters$predictionDay) | is.null(parameters$predictionDay))
  
  # Data
  reqVar <- c(parameters$publicationDateVarName,
              parameters$timeVarName,
              parameters$sampleSizeVarName,
              parameters$houseVarName)
  optVar <- parameters$periodVarNames
  partyNames <- parameters$partyVarNames
  data <- data[ ,c(reqVar, optVar, partyNames)]
  if(!is.null(optVar)) optVar <- c("periodFrom", "periodTo")
  names(data) <- c("publDate", "time", "n", "house", optVar, partyNames)
  
  missingParties <- which(rowSums(is.na(data[,partyNames])) > 0) 
  if(length(missingParties) > 0) warning("The following polls (rows) contains missing values:\n",
                                         paste(as.character(missingParties), collapse=", "))
  stopifnot(sum(is.na(data[, c("n","house", "time")])) == 0) # Test that there are no missing values
  # Test that there are no missing values in period dates if period is used
  stopifnot(length(levels(data$house)) > 0)
  stopifnot(class(data$house) == "factor")
  stopifnot(class(data) == "data.frame" & reqVar %in% names(data))
  stopifnot(length(partyNames) > 0)
  stopifnot("POSIXt" %in% class(data$time) | is.numeric(data$time))
  stopifnot(is.factor(data$house))
  stopifnot(all(rowSums(data[, partyNames]) <= 1))
  stopifnot(!any(parameters$electionDay < data$time))
  stopifnot(all(data$periodTo - data$periodFrom > 0))
  stopifnot(min(data$time) < parameters$predictionDay)
  missingOther <- which(rowSums(data[,partyNames])==1)
  if(length(missingOther)!=0) {
    data <- data[-missingOther,]
    warning("The following observations were removed since the parties sum to one\n(indicating that there is an error in this observation):\n",
            paste(as.character(missingOther), collapse=", "),
            call.=FALSE)
  }

  # Priors
  stopifnot(c("m0", "C0", "tau2matrix", "nub", "Sb") %in% names(priors))
  stopifnot(all(partyNames %in% names(priors$m0)))
  stopifnot(all(partyNames %in% colnames(priors$C0)))
  stopifnot(all(partyNames %in% rownames(priors$C0)))
  stopifnot(all(partyNames %in% colnames(priors$Sb)))
  stopifnot(all(partyNames %in% partyNames))
  stopifnot(all(levels(data$house) %in% rownames(priors$tau2matrix)))  
  stopifnot(all(rownames(priors$tau2matrix) %in% levels(data$house)))
  stopifnot(all(dim(priors$tau2matrix)==c(length(levels(data$house)),2)))
  # stopifnot(all(!is.na(priors$tau2matrix))) NA indicates tau == 1 allways
  # stopifnot(is.numeric(priors$nub) & priors$nub > ncol(priors$Sb))
  stopifnot(is.numeric(priors$nub) & priors$nub > 0)
  stopifnot(all(names(priors$m0) %in% partyNames))
  stopifnot(length(priors$m0)==ncol(priors$Sb))
  stopifnot(all(dim(priors$Sb)==dim(priors$C0)))
  stopifnot(length(priors$m0)==ncol(priors$C0))
  
  ## Rearrange priors to fit data
  priors$m0 <- priors$m0[partyNames]
  priors$C0 <- priors$C0[partyNames, partyNames]
  priors$Sb <- priors$Sb[partyNames, partyNames]
    
  # Temporary change electionDay variable
  parameters$electionDay <- parameters$electionDay - days(1)
  
  ## Create variables and data
  # Use period
  usePeriod <- !is.null(parameters$periodVarNames)
  if(usePeriod) {
    if(!parameters$silent) message("Poll period is used in the model.")
  } else {
    if(!parameters$silent) message("Poll date (time) is used in the model.")
  }
  
  # Time 
  if(usePeriod) {
    TT <- as.numeric(parameters$electionDay - min(data$periodFrom) + 1)
  } else {
    TT <- as.numeric(parameters$electionDay - min(data$time) + 1)    
  }
  
  predictionDay <- parameters$electionDay + days(1)
  if(!is.null(parameters$predictionDay)) predictionDay <- parameters$predictionDay

  # Filter out later polls 
  data <- data[data$publDate < predictionDay,]
  if(length(levels(data$house)) > length(unique(as.character(data$house)))) {
    missingHouse <- str_join(levels(data$house)[!levels(data$house)%in%unique(data$house)],collapse=", ")
    message(missingHouse, " is not found in data. These levels are removed.")
    data$house <- factor(as.character(data$house))
  }
  stopifnot(length(levels(data$house)) == length(unique(data$house)))
  stopifnot(all(table(data$house) > 0))
  priors$tau2matrix <- priors$tau2matrix[levels(data$house),]
  
  
  # Data
  if(usePeriod) expdata <- expandPollToPeriod(data) else expdata <- data

  dateVec <- parameters$electionDay-days(0:(TT-1))
  Y_all <- data.frame(time = dateVec)
  Y_all <- merge(Y_all,expdata, by="time", all.x=TRUE)
  Y_all <- Y_all[order(Y_all$time,decreasing=TRUE),]
  Y_all$tau2 <- 1  
  Y_all$n_eff <- Y_all$n / Y_all$tau2
  
  Y <- weightPolls(Ydata = Y_all, partyNames = partyNames)
  
  # Missing indicator
  missingObs <- is.na(Y_all$house[!duplicated(Y_all[,"time"])])
  missingObsAll <- is.na(Y_all$house)

  ## MCMC bookkeeping 
  gibbsTheta <- array(0, dim = c(TT + 1, length(priors$m0), parameters$MCMCsamples), 
                      dimnames=list(c(as.character(parameters$electionDay + days(1)),
                                      substr(as.character(Y$time),1,10)),names(priors$m0),NULL)) 
  gibbsTau2 <- array(0, dim = c(nrow(priors$tau2matrix), parameters$MCMCsamples),
                     dimnames=list(rownames(priors$tau2matrix), NULL)) 
  gibbsWb <- array(0, dim = c(nrow(priors$Sb), ncol(priors$Sb), parameters$MCMCsamples),
                   dimnames=list(rownames(priors$Sb), colnames(priors$Sb), NULL))

  # Starting values 
  gibbsTau2[, 1] <- priors$tau2matrix[,"beta"]/(priors$tau2matrix[,"alpha"]-1)
  gibbsWb[,, 1] <- priors$Sb

  ## Defining model
  # Make time invariant variances
  X <- timeInvX(pMatrix=Y[,partyNames],n=Y$n_eff) # Must handle missing data

  # Creating a dlm model object
  mod <- dlm(FF = diag(length(priors$m0)),
             V = .createCovMatrix(p = Y[which(!is.na(Y[,partyNames[1]]))[1], partyNames], n = Y$n_eff[which(!is.na(Y[,partyNames[1]]))[1]]),
             JV = matrix(1L:length(priors$m0)^2, ncol=length(priors$m0)),
             GG = diag(length(priors$m0)),
             W = priors$Sb,
             X = X,
             m0 = priors$m0,
             C0 = priors$C0)

  # MCMC it <- 2
  set.seed(parameters$seed)
  sampleNo <- 0
  totsamples <- (parameters$MCMCsamples * parameters$thin + parameters$burnin)
  if(!parameters$silent) pb <- txtProgressBar(min = 0, max = totsamples, style = 3)
  
  for(it in 2 : totsamples){
    saveSample <- it > parameters$burnin & it %% parameters$thin == 0
    if(saveSample){sampleNo <- sampleNo + 1}

    # Generate states - FFBS
    modFilt <- dlmFilter(y=Y[,partyNames], mod=mod, debug=FALSE, simplify = TRUE) 
    theta <- dlmBSample(modFilt)
    while(parameters$rejectElectDay && !all(sum(theta[1,])<=1)){
      theta <- dlmBSample(modFilt)
    }
    rownames(theta) <- c(as.character(parameters$electionDay + days(1)), 
                         as.character(Y$time))
    if(saveSample){gibbsTheta[,, sampleNo] <- theta}
    # First obs is State 0  
    
    # Update Wb # What does it mean that all theta are used?
    theta.center <- theta[-1, ] - theta[-nrow(theta), ]
    SSandSb <- crossprod(theta.center)/2 + priors$Sb
    mod$W <- solve(dlm::rwishart(df = priors$nub + dim(Y)[1]/2, Sigma = solve(SSandSb)))
    if(saveSample){gibbsWb[,, sampleNo]  <- mod$W}
    
    # Sample tau 
    bt <- as.data.frame(theta[-1,][!missingObs,])
    colnames(bt) <- partyNames
    if(usePeriod){
      tau2 <- 
        sampleTau2period(prior=priors$tau2matrix, 
                   Yt = Y_all[!missingObsAll,],
                   bt = bt)
    }else{
      tau2 <-
        sampleTau2(prior=priors$tau2matrix, 
                   Yt = Y_all[!missingObsAll,],
                   bt = bt)      
    }
    if(saveSample){gibbsTau2[, sampleNo]  <- tau2}

    # Weigh together samples + exand to use all data points
    Ytemp <- merge(Y_all[!missingObsAll, c("time","house")], data.frame(house=names(tau2),tau2=tau2), by="house")  
    Ytemp <- Ytemp[order(Ytemp$time,decreasing=TRUE),]
    Y_all[!missingObsAll,"tau2"] <- Ytemp$tau2
    Y_all$n_eff <- Y_all$n / Y_all$tau2
    Y <- weightPolls(Ydata = Y_all, partyNames = partyNames)
    
    # Update V
    mod$X[!missingObs,] <- timeInvX(Y[!missingObs, partyNames], Y$n_eff[!missingObs])

    # Iteration
    if(!parameters$silent) setTxtProgressBar(pb, it)
  }
  if(!parameters$silent) close(pb)

  # Temporary change electionDay variable
  parameters$electionDay <- parameters$electionDay + days(1)
  
  # Calculate running time  
  runtime <- Sys.time() - timeStamp
  
  # Save results
  results <- list(bt = gibbsTheta, Wb = gibbsWb, tau2 = gibbsTau2, param=parameters, priors=priors, data = data, runtime=runtime)  
  class(results) <- "ElectPred"
  return(results)
}

