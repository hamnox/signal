library("ggplot2")

getSamples = function(a, n) {
  b = sqrt(1 - (a^2))
  xs = rnorm(n)
  error = rnorm(n, sd = b)
  ys = (a * xs) + error
  return(data.frame(xs,ys))
}

as = seq(0.1,0.9,0.1)
ns = c(100, 500, 2500, 10000)

df = getSamples(.97, 10000)
p = ggplot(df, aes(df$xs, df$ys))
p + geom_point(alpha=0.01) + geom_smooth(method="lm")


estimateSlopes = function(a,n,numTrials = 500){
  new_vec = c()
  for (i in 1:numTrials){
    test = getSamples(a,n)
    a_est = coef(lm(test$ys ~ test$xs))[2]
    names(a_est)=NULL
    new_vec = c(a_est, new_vec)
  }
  return (new_vec)
}

mean(estimateSlopes(0.8,200))

##Histogram
df2 = data.frame(estimateSlopes(0.8,200, numTrials = 1000))
p2 = ggplot(df2, aes(x=df2[1]))+geom_histogram()
p2



dfSD = c()

for (n in ns) {
  new_col = sapply(as, function(a) {
    result = sd(estimateSlopes(a, n))
    return(result)
  })
  print(new_col)
  dfSD = cbind(dfSD, new_col)
}

dfSD
as.data.frame(dfSD)
rownames(dfSD) = as
colnames(dfSD) = ns
dfSD


dfSD2 = c()
new_ns = seq(1,100,5)
new_col = c()
a = 0.1
for (n in new_ns) {
  result = sd(estimateSlopes(a, n))
  new_col = c(new_col, result)
  dfSD2 = data.frame(new_col)
}

dfSD2
dfSD2 = cbind(new_ns,dfSD2)
as.data.frame(dfSD2)
plot(dfSD2)
ggplot(dfSD2, aes(x=new_col, y=new_ns))+geom_smooth()


estimateSlopeswithPvals = function(a,n,numTrials = 500){
  new_avec = c()
  new_pvec = c()
  for (i in 1:numTrials){
    test = getSamples(a,n)
    test_lm = (lm(test$ys ~ test$xs))
    a_est = coef(test_lm)[2]
    p_val = summary(test_lm)$coefficients[,4][2]
    print(p_val)
    names(a_est)=NULL
    names(p_val)=NULL
    new_avec = c(a_est, new_avec)
    new_pvec = c(p_val, new_pvec)
  }
  return (data.frame(new_avec, new_pvec))
}
pval = estimateSlopeswithPvals(0.2,300)
ggplot(pval, aes(x=new_avec, y=new_pvec))+geom_point(alpha=0.5)
ggplot(pval, aes(x=new_avec))+geom_histogram()
mean(pval$new_avec)
tester2 = summary(lm(mtcars$mpg~mtcars$cyl))

lowpval = estimateSlopeswithPvals(0.1,500, numTrials=10000)
ggplot(lowpval, aes(x=new_avec, y=new_pvec))+geom_point(alpha=0.01)

median(lowpval$new_pvec)
length(lowpval$new_avec[lowpval$new_avec <= 0]) / length(lowpval$new_avec)
