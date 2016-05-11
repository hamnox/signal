library("dplyr")
library("ggplot2")
# install.packages("Rmisc")
library("Rmisc")

set.seed(1)
j = 50
a = 0.25
x = rnorm(j)
error = sqrt(1 - a^2) * rnorm(j)
y = a*x + error

summary(lm(y ~ x - 1))

cost = function(x, y, aEst, lambda, p) {
  # Two vectors x and y of equal length
  # An estimate of the value of a , aEst
  # A regularization parameter lambda
  # A number p = 1 or 2, indicating whether L 1 or L 2 regularization is being
  # performed
  
  # cost function=sum (predicted-actual)^2 + abs(coefficients)^p
  estimated = aEst*x
  SSE = sum((y - estimated)^2)
  regularize = lambda * abs(aEst)^p
  return(SSE + regularize)
}

df = expand.grid(2^seq(-5,4,1), seq(-0.1, 0.3, 0.001))
names(df) = c("lambda", "a")



df$costl1 = unlist(Map(function (l, a) {
  cost(x, y, aEst=a, lambda=l, p=1)  
  },
  df$lambda, df$a))

df$costl2 = unlist(Map(function (l, a) {
  cost(x, y, aEst=a, lambda=l, p=2)  
},
df$lambda, df$a))

plots = lapply(unique(df$lambda),
                  function (l) {
                    ggplot(data=filter(df, lambda==l), aes(x=a)) + geom_line(aes(y=costl1), alpha=0.5, color="red") + geom_line(aes(y=costl2), alpha=0.5, color="green")
                  })

multiplot(plotlist=plots, cols=2)
# ggplot(data=filter(df, lambda==2), aes(x=a)) + geom_line(aes(y=costl1), alpha=0.5, color="red") + geom_line(aes(y=costl2), alpha=0.5, color="green")


# clear vars now
# comparing regularization vs. stepwise regression

df = read.csv("signal/speedDatingSimple.csv")
