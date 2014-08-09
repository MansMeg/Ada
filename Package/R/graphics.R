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




