# Resampling
library("dplyr")
library("ggplot2")

df = read.csv("signal/speedDatingSimple.csv")
getwd()
str(df)
# n needs to be the number of data points you have
# We conjecture each increment in n to give us another inflection point
# so for case: 1 data point, you need the intercept
# Case  N-req
#  1     1
#  2     2    (a0 + a1x)
#  3     3    ..probably? (a0 + a1x + a2x^2)

x^2 polynomial
1 inflection points
3 data points
first two can be fitted by either a positive or negative slope
3rd either needs or does not need an inflection point
you always get n-1 as the max requirement

# n  eq
# 1  y = b .. 7 = 7 = a0
# 2  y = mx + b .. # guaranteed a line

set.seed(1992)
runif(5)

str(df)

# we'll do the 1th gender
df$gender
males =  filter(df, gender == 1)

fit = lm(attr_o ~ .-X -gender -attr_o - sinc_o -intel_o - fun_o -amb_o,  data=males)
summary(fit)
names(fit)
coeffs = sort(fit$coefficients, decreasing=TRUE)


# Stuff that was positively correlated
# music
# sports (but not watching sports)
# exercise
# dining

# Negatively
# theater
# concerts
# museums p value .299696 though..
# tv
# gaming

set.seed(512)
testifier = function(x) {
  idx = base::sample(1:nrow(males), round(nrow(males)/2))
  test = males[idx,]
  train = males[-idx,]
  # test = sample_n(males, round(nrow(males)/2,0))
  # oops, calling length instead of nrows() caused big panic
  trainfit = lm(attr_o ~ .-X -gender -attr_o - sinc_o -intel_o - fun_o -amb_o, data=train)
  # plotty = mutate(test, predicty = )
  test_predictions = predict(trainfit, test)
  test_correlation = cor(test_predictions, test$attr_o) ^ 2
  train_predictions = predict(trainfit, train)
  train_correlation = cor(train_predictions, train$attr_o) ^ 2
  return(c(train_correlation, test_correlation))
}
example = testifier(males)
rsquareds = t(lapply(1:100, testifier))
str(rsquareds)
head(rsquareds)
data.frame(rsquareds)
p = ggplot(data.frame(rsquareds), aes(x=rsquareds))
p + geom_density()

sd(rsquareds) / sqrt(nrow(test))
