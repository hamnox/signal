# install.packages(c("bigmemory", "biganalytics"))
library("bigmemory")
library("biganalytics")

options(digits = 10)

# load a big matrix, create a backing and description file
x <- read.big.matrix("datasets/2008.csv", type="integer", header=TRUE,
                     backingfile="airline.bin",
                     descriptorfile="airline.desc",
                     extraCols="Age")
round(summary(x))



birthmonth <- function(y) {
  minYear <- min(y[,'Year'], na.rm=TRUE)
  these <- which(y[,'Year']==minYear)
  minMonth <- min(y[these,'Month'], na.rm=TRUE)
  return(12*minYear + minMonth - 1)
}

# install.packages("bigtabulate")
library(bigtabulate)

# not working for some reason
planeindices <- bigsplit(x, 'TailNum')

# using this instead
planeindices <- split(1:nrow(x), x[,'TailNum'])

planeStart <- sapply(planeindices, function(i) birthmonth(x[i, c('Year', 'Month'), drop=FALSE]))


# alternative with cores
# install.packages("doMC")
library("doMC")
detectCores() # 8 cores
registerDoMC(cores=8)
planeStart <- foreach(i = planeindices, .combine=c) %dopar% {
  return(birthmonth(x[i, c('Year', 'Month'), drop=FALSE]))
}

# calculate the age of the plane at each flight
# Issue: this just gets NAs for Age. 
x[,'Age'] <- x[,'Year']*as.integer(12) + x[,'Month'] - as.integer(planeStart[x[,'TailNum']])

x[,'Age'] <- x[,'Year']*as.integer(12) +
  x[,'Month'] - as.integer(planeStart[x[,'TailNum']])

head(x[!is.na(x[,'TailNum']), 'Age'])

# Big Regression
blm <- biglm.big.matrix(ArrDelay ~ Age + Year, data=x)
summary(blm)
