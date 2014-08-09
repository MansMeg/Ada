#' @title
#' plot.ElectPred
#' 
#' @details
#' Plots how the states and polls over time for each party
#' 
#' @param x An ElectPredict object
#' @param party Which party to plot
#' @param showPriors Should the prior on electionday be shown?
#' @param intervals The probability intervals to be calculated
#' @param returnPlots Don't print the plot, return them as list withh ggplot objects
#' @param ... not currently in use.
#' 
#' @return
#' If returnPlots=TRUE returneras en lista med ggplotobjekten.
#'
#' @export

plot.ElectPred <- function(x, party = "all", showPriors=TRUE, intervals=c(0.95,0.75), returnPlots=FALSE, ...){
  # Assertions
  require(ggplot2)
  stopifnot(class(x) == "ElectPred")
  stopifnot(length(intervals) > 0 & class(intervals) == "numeric")
  stopifnot(party == "all" | party %in% dimnames(x$bt)[[2]])
  
  # Some corrections
  obj <- x  
  noIntrv <- length(intervals)
  
  # Choose parties to plot
  if(party == "all") party <- dimnames(obj$bt)[[2]]

  # Calculate quantiles
  partyPlot <- .calcQuantilesTime(obj,intervals=intervals)
  
  if(!is.null(x$param$electionResults)){
    trueValues <- x$param$electionResults[party]
  }else{
    trueValues <- numeric(0)
  }
  
  ask <- prod(par("mfcol")) < length(party) && dev.interactive()
  if (ask) {
    oask <- devAskNewPage(TRUE)
    on.exit(devAskNewPage(FALSE))
  }  
  colorDF <-
    data.frame(color=c("blue", "blue", "dark green", "red", "dark red",
                     "dark blue", "dark green", "blue"))
  rownames(colorDF) <- c("M","FP","C","S","V","KD","MP","SD")

  # Do plots
  plotList <- list()
  for (j in 1:length(party)){ #j <- 1
    myCol <- as.character(colorDF[names(partyPlot)[j],1])        
    plotData <- as.data.frame(partyPlot[[j]])
    names(plotData)[noIntrv + 1] <- "medianPred"
    for(i in noIntrv:1){ # i <- 1
      names(plotData)[c(i, 2*noIntrv + 2 - i)] <- paste(c("low","high"),i,sep="")
    }
    plotData$time <- suppressWarnings(ymd(rownames(plotData)))
    plotData$time[1] <- plotData$time[2] + days(1)
    
    minPoll <- min(obj$data[,names(partyPlot)[j]])
    maxPoll <- max(obj$data[,names(partyPlot)[j]])    
    minBand <- min(plotData[,1])
    maxBand <- max(plotData[,ncol(plotData)-1])
    minPrior <- qnorm(p= (1 - intervals)/2 , x$priors$m0[j], sqrt(x$priors$C0[j, j]))
    maxPrior <- qnorm(p= 1 - (1 - intervals)/2 , x$priors$m0[j], sqrt(x$priors$C0[j, j]))
    ylimit <- c(min(minPoll, minBand, min(minPrior), trueValues[j], na.rm=TRUE), 
                max(maxPoll, maxBand, max(maxPrior), trueValues[j], na.rm=TRUE))
    ylimit <- c(0.98, 1.02) * ylimit
    
    ggplotObj <- 
      ggplot(plotData, aes(x=time)) +
      theme_bw() + 
      coord_cartesian(ylim = ylimit)
    for(i in 1:noIntrv){ # i <- 1
      ggplotObj <- ggplotObj + 
        geom_ribbon(aes_string(ymin=paste("low",i,sep=""), ymax=paste("high",i,sep="")), alpha=0.20, fill=myCol)
    }
    ggplotObj <- ggplotObj + geom_line(aes(y=medianPred), color=myCol)

    # Add true value
    moveRightDays <- 5
    if(!is.null(trueValues)){

      ggplotObj <- ggplotObj + geom_point(data=data.frame(x=x$param$electionDay + days(moveRightDays),
                                                          y=trueValues[j]), 
                                          aes(x=x, y=y), alpha = 1, color=myCol, shape=4, size=3)
    }
    
    # Add prior 
    if(showPriors){
      priorRanges <- data.frame(low=minPrior[order(minPrior)],
                                high=maxPrior[order(maxPrior,decreasing=TRUE)],
                                x=x$param$electionDay + days(moveRightDays))
      ggplotObj <- ggplotObj + geom_linerange(data=priorRanges, 
                                            aes(x=x, ymin=low,ymax=high), alpha = 0.2, color=myCol)
      ggplotObj <- ggplotObj + geom_point(data=data.frame(x=x$param$electionDay + days(moveRightDays),
                                                          y=x$priors$m0[j]), 
                                              aes(x=x, y=y), alpha = 1, color=myCol)
    }    

    ggplotObj <- ggplotObj + 
      geom_point(data=obj$data, aes_string(x = "time", y = names(partyPlot)[j]), shape=3, color="black")

    ggplotObj <- ggplotObj + 
      labs(x="Date", y=paste("Support for", names(partyPlot)[j], "(%)"))

    # Add if period, show the periods
    if(!is.null(x$param$periodVarNames)){
      PeriodDF <- data.frame(x = x$data$periodFrom,
                             y = x$data[,party[j]],
                             xend = x$data$periodTo)
      
      ggplotObj <- ggplotObj + geom_segment(data=PeriodDF, 
                                            aes(x=x,xend=xend,y=y,yend=y), 
                                            alpha = 0.8, linetype=3)
    }
    
    plotList[[j]] <- ggplotObj
    if(!returnPlots) plot(ggplotObj)
  }
  
  if(returnPlots){
    return(plotList)
  }else{
    return(invisible(NULL))
  }
}


#' @title
#' plotHouses
#' 
#' @details
#' Plot the design effects of different polling houses.
#' 
#' @param x An ElectPredict object
#' @param returnPlots Don't print the plot, return them as list withh ggplot objects
#' @param ... not currently in use.
#' 
#' @return
#' If returnPlots=TRUE a list with ggplots are return.
#'
#' @export

plotHouses <- function(x, returnPlots=FALSE, ...){
  require(ggplot2)
  stopifnot(class(x) == "ElectPred")
  
  # Create data
  houseNames <- rownames(x$tau2)
  known <- character(0)
  for (i in seq_along(houseNames)){
    if(all(head(x$tau2[i,],n=100)==1)) {
      known <- c(known, houseNames[i])
      next()
    }
    df <- data.frame(house=houseNames[i],
                     tau2=x$tau2[i,], 
                     stringsAsFactors=FALSE)
    if(i == 1) dfall <- df
    if(i > 1) dfall <- rbind(dfall, df)
  }

  pl <- ggplot(dfall, aes(x=tau2)) + theme_bw() + 
    geom_density(aes(group=house, colour=house, fill=house), alpha=0.3)    
  
  if(returnPlots){
    return(pl)
  }else{
    plot(pl)
    return(invisible(NULL))
  }
}


#' @title
#' plotEvaluation
#' 
#' @details
#' Plot evaluation plots of an "ElectPred" object.
#' 
#' @param x An ElectPredict object
#' @param returnPlots Don't print the plot, return them as list withh ggplot objects
#' @param ... not currently in use.
#' 
#' @return
#' If returnPlots=TRUE 
#'
#' @export

plotEvaluation <- function(x, returnPlots=FALSE, interval=0.9, combinations=list(c("S", "MP", "V"), c("M","FP","C","KD")), ...){
  require(ggplot2)
  stopifnot(class(x) == "ElectPred")  

  lowLim <- (1 - interval)/2
  posterior <- x$bt[1,,]

  combSums <- numeric(0)
  if (length(combinations) > 0){
    for(comb in combinations){
      posterior <- rbind(posterior, colSums(posterior[comb,]))
      rownames(posterior)[length(rownames(posterior))] <- paste(comb, collapse="+")
      if(!is.null(x$param$electionResults)) {
        combSums <- c(combSums, sum(x$param$electionResults[comb]))
      }else{
        combSums <- c(combSums, sum(x$priors$m0[comb]))        
      } 
    }
  }
  dat <- data.frame(t(apply(posterior,1,quantile,probs=c(lowLim, 1-lowLim))))
  colnames(dat) <- c("low", "high")
  dat$predWidth <- dat$high - dat$low
  if(!is.null(x$param$electionResults)) {
    dat$true <- c(x$param$electionResults[rownames(x$bt[1,,])], combSums)
    } else {
    dat$true <- c(x$priors$m0, combSums)
    }
  dat$Parti <- rownames(dat)
  if(!is.null(x$param$electionResults)) dat$correct <- dat$true < dat$high & dat$true > dat$low 
  dat$quantile <- 0
  for(i in seq_along(dat$true)){
    dat$quantile[i] <- quantile(posterior[i, ], probs=dat$true[i])    
  }

  g <- ggplot(dat, aes(x=Parti, y=true*100, ymin = low*100, ymax=high*100, colour = Parti)) + 
    geom_linerange() + geom_point(size=3, shape=4) + geom_pointrange(shape=4) + xlab("Parti") + ylab("%")     
    if(!is.null(x$param$electionResults)) {
      g <- g + ggtitle(paste("Prediktionsinterval", interval*100, "% och utfall", as.character(x$param$electionDay)))
    }else{
      g <- g + ggtitle(paste("Prediktionsinterval", interval*100, "% och prior (m0)", as.character(x$param$electionDay)))      
      names(dat)[4] <- "prior"
    }
  if(returnPlots){
    return(g)
  }else{
    plot(g)
    return(dat)
  }
}



