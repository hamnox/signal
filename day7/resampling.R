# Resampling
library("dplyr")
library("ggplot2")

df = read.csv("signal/speedDatingSimple.csv")
getwd()
str(df)
# Resampling
library("dplyr")
library("ggplot2")

# n needs to be the number of data points you have
# We conjecture each increment in n to give us another inflection point
# so for case: 1 data point, you need the intercept
# Case  N-req
#  1     1
#  2     2    (a0 + a1x)
#  3     3    ..probably? (a0 + a1x + a2x^2)

# x^2 polynomial
# 1 inflection points
# 3 data points
# first two can be fitted by either a positive or negative slope
# 3rd either needs or does not need an inflection point
# you always get n-1 as the max requirement

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

# Mel's rewrite
testifier = function(seed,usetest = TRUE) {
  set.seed(seed)
  idx = base::sample(1:nrow(males), round(nrow(males)/2))
  test = males[idx,]
  train = males[-idx,]
  # test = sample_n(males, round(nrow(males)/2,0))
  # oops, calling length instead of nrows() caused big panic
  trainfit = lm(attr_o ~ .-X -gender -attr_o - sinc_o -intel_o - fun_o -amb_o, data=train)
  # plotty = mutate(test, predicty = )
  if (usetest) {
    test_predictions = predict(trainfit, test)
    correlation = cor(test_predictions, test$attr_o) ^ 2
  } else {
    train_predictions = predict(trainfit, train)
    correlation = cor(train_predictions, train$attr_o) ^ 2
  }
  return(correlation)
}
example = testifier(13)
set.seed(7)
seedslist = round(runif(100)*1000,0)
testrsquared = sapply(seedslist, testifier, usetest=TRUE)
trainrsquared = sapply(seedslist, testifier, usetest=FALSE)
qplot(testrsquared, trainrsquared)

sd(testrsquared) / sqrt(138) #0.003220488
sd(trainrsquared) / sqrt(138) #0.004323359


subsetter = function(n, df) {
  # will pad with stuff
  indices = sample(1:nrow(df), nrow(df))
  indices = matrix(indices, ncol = n)
  lapply(1:n, function(coli) {
    return(df[indices[,coli],])
  })
  # return(list(subsample dfs))
}
results = subsetter(3, males)

trainer = function(formula, subsets) {
  #str(Reduce(rbind, results[-2], data.frame()))
  results = vector(mode="list", length = length(subsets))
  for (i in seq(subsets)) {
    test = subsets[[i]]
    train = Reduce(rbind, subsets[-i], data.frame())
    # not sure.. double bracket? How to assign to lists without everything blowing up?
    model = lm(as.formula(formula), data=train)
    predicted_vals = predict(model, test)
    results[[i]] = data.frame(y = test$attr_o, yhat = predicted_vals)
  }
  all_examples = Reduce(rbind, results, data.frame())
  quality = cor(all_examples$y, all_examples$yhat) ^ 2
  return(quality)
}

example2 = trainer("attr_o ~ . -attr_o", results)
example2 = trainer("attr_o ~ exercise + concerts + music", results)
example2

# 2-fold
results100 = vector(length=100)
for (i in 1:100) {
  results100[i] = trainer("attr_o ~ exercise + concerts + music", subsetter(2, males))
}
results100
data.frame(results100)
ggplot(data.frame(results100)) + geom_density(aes(x=results100))
sd(results100) / sqrt(nrow(males)) # 0.0004058836
mean(results100) # 0.09078468

# 10-fold
results100 = vector(length=100)
for (i in 1:100) {
  results100[i] = trainer("attr_o ~ exercise + concerts + music", subsetter(10, males))
}
results100
ggplot(data.frame(results100)) + geom_density(aes(x=results100))
sd(results100) / sqrt(nrow(males)) # 0.000395185
mean(results100) # 0.09582586

# the standard error grows smaller with increase in n
# the distribution of the associated R^2 .. the spread seems to decrease with increase in n
# also seems more.. normal?


# Stepwise regression

# for (all of the predictors) > (1 predictor)

# formula (attr_o ~ paste(, "+", "-")) things whatever
#   get the r2 for the model using those predictors
#   fit model with whole data set, highest p-value
#   see which predictor to get rid of
#   store the r2 with number of features moved
#     on next iteration, all of predictors[-gotten rid of predictors]

library("dplyr")
cleanedup = select(df, -X, - sinc_o, -intel_o, - fun_o, -amb_o)
n=10
stepwise_regress = function(cleanedup, n=10) {

  removed = rep(NA, length(cleanedup))
  removed[1] = 'attr_o'
  rsquareds = matrix(vector(length=2*(length(removed))), ncol=2)
  for (i in seq(removed)[-1]) {
    curr_formula = paste("attr_o ~ .",
                         Reduce(function(a, x) paste(a, x, sep=" -"), removed[!is.na(removed)],""),
                         sep="")
    subsets = subsetter(n, cleanedup)
    quality = trainer(curr_formula, subsets)
    curr_model = lm(curr_formula, cleanedup)
    # part of return will be 1 thing removed, r2 = quality
    rsquareds[i-1,] = c(sum(!is.na(removed)), quality)
    coefficients = summary(curr_model)$coefficients[,4][-1] #4th column, minus intercept
    coefficients = coefficients[order(coefficients, decreasing=TRUE)]
    removed[i] = names(coefficients[1])
  }
  rownames(rsquareds) = removed
  colnames(rsquareds) = c("nremoved", "rsquared")
  rsquareds[nrow(rsquareds),] = c(sum(!is.na(removed)),
                              trainer("attr_o ~ -.", subsets))
  return(rsquareds)
}
rsquareds = stepwise_regress(cleanedup)

#           nremoved   rsquared
# attr_o          1 0.14119105
# art             2 0.15020422
# hiking          3 0.15826992
# reading         4 0.15080737
# movies          5 0.15876081
# shopping        6 0.15707776
# museums         7 0.15380394
# yoga            8 0.16693542
# concerts        9 0.16396509
# tv             10 0.15544465
# music          11 0.15180228
# theater        12 0.15276739
# dining         13 0.15176848
# gaming         14 0.15350751
# tvsports       15 0.13413022
# clubbing       16 0.12771183
# sports         17 0.10793903
# gender         18 0.05146804
# exercise       18 0.02160993

# playing around
base_plot = ggplot(cleanedup, aes(color=round(attr_o/3)))
base_plot + geom_jitter(aes(x=yoga, y=shopping))

# playing around with actual question, plotting as requested
ggplot(data.frame(rsquareds), aes(x=nremoved, y=rsquared)) + geom_point() + geom_smooth()
# this no longer valid since I encapsulated the function

cor(cleanedup$gender, cleanedup)
# lowest two cors are shopping, theater
# highest are sports, gaming
# closest to zero is exercise...
# In the future I think what I should do is check cor(base, variable) * sd(variable)
cleanedup %>%
  mutate(gshop = gender*shopping,
         gtheatre = gender*theater,
         gsports = gender*sports,
         ggame = gender*gaming,
         gexercise = gender*exercise) -> cleanedup2
rsquareds = stepwise_regress(cleanedup2)

# nremoved    rsquared
# attr_o           1 0.143369966
# gaming           2 0.168072528
# gexercise        3 0.163014432
# hiking           4 0.158984559
# art              5 0.158506997
# gender           6 0.167979656
# reading          7 0.165205329
# theater          8 0.174849780
# gshop            9 0.169660275
# museums         10 0.183372584
# shopping        11 0.184239529
# movies          12 0.177335963
# yoga            13 0.181470317
# concerts        14 0.172610579
# tv              15 0.177791728
# music           16 0.173742104
# dining          17 0.166994654
# sports          18 0.152314973
# tvsports        19 0.163786916
# ggame           20 0.142590229
# gsports         21 0.133691677
# clubbing        22 0.120183974
# exercise        23 0.069667920
# gtheatre        24 0.002296865

# exploring top var*var predictors
ggplot(data.frame(rsquareds), aes(x=24-nremoved, y=rsquared)) + geom_point() + geom_smooth()
ggplot(cleanedup2, aes(group=gender, x=theater, y=attr_o, color=gender)) + geom_jitter() + geom_smooth(method=lm)

males = filter(df, gender == 1)
females = filter(df, gender == 0)
# using step
stepwiseofficial = step(lm(attr_o ~ sinc_o + intel_o + fun_o + amb_o, data=males),
     formula(attro_o ~ -., data=males),
     direction="backward")
# - sinc_o, -intel_o, - fun_o, -amb_o


stepwiseofficial
male_coeffs = stepwiseofficial$coefficients
# (Intercept)       fun_o 
# 1.030100    0.774327 
# average .7 more points attractive for each point more fun
stepwiseofficial_fe = step(lm(attr_o ~ sinc_o + intel_o + fun_o + amb_o, data=females),
                        formula(attro_o ~ -., data=females),
                        direction="backward")
female_coeffs = stepwiseofficial_fe$coefficients
# average .7 more points attractive for each point more fun, .3 more for ambition
# (Intercept)       fun_o       amb_o 
# -0.06867023  0.68495181  0.31024920 

# yup that's pretty correlated! next closest is kinda pretty correlated
ggplot(males, aes(x=fun_o, y=attr_o, color=sinc_o)) + geom_jitter()
ggplot(females, aes(x=amb_o, y=attr_o, color=amb_o)) + geom_jitter() + geom_smooth()

# fun is definitely correlated. Less so than with males maybe, more variation
# no fun makes jack an ugly boy
# correlated with each other...
ggplot(females, aes(x=fun_o, y=attr_o, color=amb_o)) + geom_jitter()
ggplot(females, aes(x=amb_o, y= fun_o, color=attr_o)) + geom_jitter()

# fun is great
# for everyone
# but girls;
# be ambitious too
