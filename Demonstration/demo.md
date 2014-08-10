Using Botten Ada for yourself
========================================================

This is a document explaining how to run the bayesian dynamic linear model behind [bottenada.se](http://www.bottenada.se). The Program running Ada is based on [R](http://www.r-project.org/) code and is contained in an R-package on github.

Below is an example of installing and using the botten Ada model to do your own predictions of the Swedish election 2014.

## Preparations
### Installing the bottenada R-package

As the first step we need to install the package from github to [R](http://www.r-project.org/). Just fire up [R](http://www.r-project.org/) and run the following code.


```r
library(devtools)
library(lubridate)
devtools::install_github(repo = "Ada", username = "MansMeg", subdir = "Package", 
    dependencies = TRUE)
library(bottenada)
```





### Downloading polling data 

We use the polling data from Sweden that is contained in the SwedishPolls github repo. We download the data as ```polls``` the following way:


```r
devtools::source_gist("https://gist.github.com/MansMeg/c0527fd762580006daed", 
    quiet = TRUE)
```

```
## SHA-1 hash of file is e3465b3ed544ab9c6fb86a3fd9929fc2e4f51244
```

```r
polls <- source_GitHubData("https://github.com/MansMeg/SwedishPolls/raw/master/Data/Polls.csv", 
    sep = ",", dec = ".", header = TRUE)
```

```
## Loading required package: httr
```


### Some preprocessing of the data

We first choose which parties to do the analysis for as an character vector.


```r
parties <- c("M", "FP", "C", "KD", "S", "V", "MP", "SD")
```


We then create a new dataset to use for the analysis. 


```r
predData <- polls[, c(parties, "n", "house")]
predData[, parties] <- predData[, parties]/100
```


Then we add time points to use in the model and then add the ```house``` variable.


```r
predData$time <- ymd(polls$collectPeriodTo)
predData$publDate <- ymd(polls$PublDate)
predData$periodFrom <- ymd(polls$collectPeriodFrom)
predData$periodTo <- ymd(polls$collectPeriodTo)
predData$house <- factor(predData$house)
```


We now remove data with missing values (this is actually not neccessary for the timeperiod chosen below, but can be needed for other time periods).


```r
predData <- predData[!(is.na(predData$periodFrom) | is.na(predData$periodTo)), 
    ]
predData <- predData[!(is.na(predData$house) | is.na(predData$n)), ]
predData <- predData[!(is.na(predData$publDate)), ]
predData <- predData[-which(rowSums(predData[, 1:8], na.rm = TRUE) == 1), ]
```


### Choose time period to run model

We now need to choose the time period. Below I choose the same period as is used in [bottenada.se](http://www.bottenada.se).


```r
predData2014 <- predData[predData$time > ymd("2012-11-01") & !is.na(predData$time), 
    ]
```



### Choose parameter settings

The next step is to set all the parameters used by the model.


```r
parameters2014 <- list()
# Day of election
parameters2014$electionDay <- ymd("2014-09-14")
# Day of prediction NULL if fitting model with all available data, else ex.
# ymd('2014-06-01')
parameters2014$predictionDay <- NULL
# Time period variable names
parameters2014$periodVarNames <- c("periodFrom", "periodTo")
# Publication variable name (used in plot())
parameters2014$publicationDateVarName <- "publDate"
# Timepoint used if not period is used
parameters2014$timeVarName <- "time"
# Variable name for sample size
parameters2014$sampleSizeVarName <- "n"
# Variable name for polling houses
parameters2014$houseVarName <- "house"
# Variable names for period
parameters2014$periodVarNames <- c("periodFrom", "periodTo")
# Variable name for parties
parameters2014$partyVarNames <- c("M", "FP", "C", "KD", "S", "V", "MP", "SD")
# Election results (if fitting model to earlier years were the results are
# known) ex: parameters$electionResults <- c(1791766, 390804, 420524,
# 333696, 437435, 1827497, 334053, 339610) / 5960408
# names(parameters$electionResults) <- c('M', 'C', 'FP', 'KD', 'MP', 'S',
# 'V', 'SD')
parameters2014$electionResults <- NULL
parameters2014$timeInterval <- 1  # Timescale of data (number of days) Not implemented.
```


The second part of the parameters are the MCMC parameters settings. If ```rejectElectDay``` is set to ```TRUE``` rejection sampling is used so that only samples that sum to less than one at election day is accepted.


```r
parameters2014$MCMCsamples <- 1000  # MCMC Samples
parameters2014$burnin <- 500  # Burnin samples
parameters2014$thin <- 2  # Thinning 
parameters2014$seed <- as.numeric(Sys.Date())
parameters2014$silent <- FALSE  # Should the analys be done using without printing messages
parameters2014$rejectElectDay <- TRUE  # Reject samples that sum > 1 at election day
```


If you want to replicate this run exactly you need to set the seed to 16292 and the predictionday to 2014-08-10.


### Choose priors

The next part is to set the prior parameters. Most priors (execpt the prediction on election day) are vague priors in this example, but can of course be more informative and they are similar to the ones used at [bottenada.se](http://www.bottenada.se).

The first step is to set the prior for the covariance matrix for the day-to-day change in party opinion. The prior is defined as the parameters $\Phi$ and $\nu$ of an [inverse wishart](http://en.wikipedia.org/wiki/Inverse-Wishart_distribution) distribution. 

As an example of a vague prior we set $\nu=10$ and used the covariance of the polls during 2013 to set the covariance matrix. 


```r
priorData <- polls[ymd(polls$PublDate) > ymd("2013-01-01") & ymd(polls$PublDate) < 
    ymd("2013-12-31") & !is.na(polls$PublDate), parameters2014$partyVarNames]/100
priors2014 <- list()
priors2014$nub <- 10
priors2014$Sb <- cov(priorData)
```


The next step is to set the priors for the election. This can be done in any type of way. We used our model based mainly on previous election results from Statistics Sweden and incorporating small party effects. We tried different ways to asses the priors based on the previous elections 2010 and 2006 to put the probability mass higher for smaller parties.

You need to define your prior for the election in two steps. First give your best estimates where you believe the election will end. Then you need to specify this as a covariance matrix. In Ada we specify our priors the following way (as of august 9).


```r
priors2014$m0 <- c(0.227, 0.0665, 0.0645, 0.0595, 0.353, 0.08, 0.08, 0.081)
priors2014$C0 <- diag(c(0.001024, 0.000144, 0.000144, 0.000144, 0.001024, 0.000625, 
    0.000625, 0.000625))
colnames(priors2014$C0) <- rownames(priors2014$C0) <- names(priors2014$m0) <- c("M", 
    "FP", "C", "KD", "S", "V", "MP", "SD")
```


The house effects also needs priors. Setting the house effects to ```NA``` means that we use this polling agency as "anchor" with the 'design effect' set to 1. In [bottenada.se](http://www.bottenada.se) we use Statistics Sweden (SCB) as "anchor".


```r
priors2014$tau2matrix <- matrix(data = c(1, 1), ncol = 2, nrow = length(levels(polls$house)), 
    dimnames = list(levels(polls$house), c("alpha", "beta")), byrow = TRUE)
priors2014$tau2matrix["SCB", ] <- c(NA, NA)
```


Now we are done with setting the priors and can start the analysis.

### Do the analysis

To run the MCMC sampler we just run the electPredict function in the bottenada package. 


```r
prediction2014 <- electPredict(priors = priors2014, parameters = parameters2014, 
    data = predData2014)
```








It will take some time, for me it took ``48`` minutes to run the analysis with the parameters set above.

### Check the results

Once the analysis has been run we can check the results. We should [check the convergence](http://www.people.fas.harvard.edu/~plam/teaching/methods/convergence/convergence_print.pdf) of the chains to assess that they have converged. 

The resulting object contains the chains as separate matrices/arrays in the ```prediction2014```, see ```?electPredict``` for details.

We can also get some summary statistics of the analysis with ```summary()``` and get plots with ```plot()```.


```r
plot(prediction2014)
```



```r
summary(prediction2014)
```

```
## ElectPred class object:
## Election (at 2014-09-14) predicted at .
## A total of 150 polls from 9 houses (starting at 2012-10-03).
## MCMC: 1000 samples with a burnin of 500 and a thinning of 2.
## Total runtime: ~ 48 minutes.
## 
## 
## Expected results:
##       Expected results
## M              0.21583
## FP             0.06388
## C              0.05762
## KD             0.04960
## S              0.32987
## V              0.07431
## MP             0.09408
## SD             0.08131
## Other          0.03349
## 
## 
## Probability Intervals (95%):
##           2.5%     50%   97.5%
## M     0.172098 0.21581 0.25545
## FP    0.046142 0.06398 0.08175
## C     0.042530 0.05708 0.07358
## KD    0.035040 0.04958 0.06482
## S     0.293939 0.32986 0.36749
## V     0.049856 0.07428 0.09939
## MP    0.066746 0.09430 0.12225
## SD    0.049351 0.08163 0.11461
## other 0.005372 0.03258 0.06498
## 
## 
## Parliment results probability (with limit set to 0.04):
##       2.5%     50%   97.5%
## M  0.17983 0.22396 0.26690
## FP 0.04766 0.06659 0.08417
## C  0.04378 0.05939 0.07689
## KD 0.00000 0.05130 0.06718
## S  0.30288 0.34284 0.38326
## V  0.05156 0.07728 0.10377
## MP 0.06987 0.09791 0.12710
## SD 0.05122 0.08448 0.11970
```

