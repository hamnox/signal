# library(readr)
library(ggplot2)
# install.packages("gridExtra")
library(gridExtra)
library(dplyr)
# install.packages("glmnet")
library(glmnet)

df = read.csv("day8/training.csv")
n = names(df)
bands = select(df, starts_with("m"))
targets = select(df, Ca:Sand)
others = select(df, BSAN:Depth)

biggestCors = function(name,
                       df1 = bands, 
                       df2 = targets, 
                       sign = 1, number = 5){
  cors = cor(df1, df2)
  corDF = as.data.frame(cors)
  corDF = corDF[order(-sign*corDF[name]),]
  head(corDF[name], number)
}


print("negative correlations")
lapply(names(targets), function(name){biggestCors(name, sign = -1)})

print("positive correlations")
lapply(names(targets), function(name){biggestCors(name, sign = 1)})

#Plot wave numbers
bandNames = names(bands)
waveNums = as.numeric(sapply(bandNames, function(x){
  as.numeric(substr(x, 2, 10))
}))
qplot(waveNums)



#Plot correlations with targets against wave numbers
cors = cor(bands, targets)
corDF = as.data.frame(cors)
plots = lapply(names(targets), function(target){qplot(x = waveNums, 
                                                      y = corDF[[target]] ,
                                                      geom = "point", 
                                                      xlab = "Wave Number",
                                                      ylab = names(corDF[target]))})
plots[[1]]
do.call(grid.arrange, plots)


#Plot coefficients of L^2 regularization along with correlations
m = cv.glmnet(scale(bands),targets$Ca, family ="gaussian", alpha = 0)
ndf = data.frame(waveNums, corrs = cor(bands,targets$Ca)[,1],coefficients = coef(m, m$lambda.min)[-1,1])
ggplot(ndf) + geom_point(aes(x = waveNums, y = corrs, colour="#000099")) + geom_point(aes(x = waveNums, y = 50*coefficients, colour="#CC0000", alpha = 0.1))

# Plot L1 reg'd coefficients along with correlations
m1 = cv.glmnet(scale(bands),targets$Ca, family ="gaussian", alpha = 1)
ndf$coef1 = coef(m1, m1$lambda.min)[-1,1]
(
ggplot(ndf) + geom_point(aes(x = waveNums, y = corrs, colour="#000099"))
+ geom_point(aes(x = waveNums, y = 50*coefficients, colour="#CC0000", alpha = 0.1))
+ geom_point(aes(x = waveNums[coef1 !=0], y = coef1[coef1 !=0], colour="#999900", alpha = 0.2), data = filter(ndf, coef1 != 0))
)


# assignment: see how coefficients change with alpha = 
alphas = c(1, 0.1, 0.05, 0.01, 0.001, 0)
coeffs = sapply(alphas, function(a) {
  # a = 0
  m = cv.glmnet(scale(bands),targets$Ca, family ="gaussian", alpha = a)
  coefficients = coef(m, m$lambda.min)[-1,1]
  print(paste("done with alpha =",a))
  return(coefficients)
}) # returns a matrix! HALLELUJAH!
colnames(coeffs) = c("a1-", "a1", "a05", "a01", "a001", "a0")
ndf = as.data.frame(cbind(waveNums, corrs = cor(bands,targets$Ca)[,1], coeffs))

baseplot = ggplot(ndf, aes(x = waveNums)) + geom_point(aes(y = corrs, colour="#000099"))
i = 3
for (name in names(alphas)) {
  print(paste("#33",as.hexmode(i*), "33", sep=""))
  #baseplot = baseplot + geom_point(aes_string(y="a1", colour=thecol))
  i <- i + 1
}
