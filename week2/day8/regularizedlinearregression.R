library("dplyr")
library("ggplot2")
library("Rmisc")
library("glmnet")

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
males = filter(df, gender == 1)
males = males[-(1:2)]
stepwiseofficial = step(lm(attr_o ~ . -attr_o, data=males),
                        formula(attr_o ~ -., data=males),
                        direction="backward")

# attr_o ~ fun_o + sports + tvsports + exercise + gaming + tv + 
#   theater + shopping

stepwiseofficial$coefficients
 
# (Intercept)       fun_o      sports    tvsports    exercise      gaming          tv     theater 
# 1.47449609  0.70925470  0.06414338 -0.03480081  0.05470115 -0.04911406 -0.03615014 -0.07313221 
# shopping 
# 0.03902577

# woops did mean instead of sum there.
sqrt(mean(stepwiseofficial$residuals ^ 2))

l1g = glmnet(x=as.matrix(males[-1]), y=males$attr_o, alpha=1)
l2g = glmnet(x=as.matrix(males[-1]), y=males$attr_o, alpha=0) # WHAT WHY IS IT ZERO
# no idea what we're supposed to be looking for in lambda, but here they are!
l1g$lambda

inputs = as.matrix(males[-1])

getbestlambda = function(object, inputs, truevals) {
  predictions = predict(object, newx=inputs, s=object$lambda)
  colnames(predictions) = object$lambda
  rmses = sqrt(colMeans((predictions - truevals) ^ 2))
  lambda = object$lambda[order(rmses)][1]
  rmse = min(rmses)
  return(c(lambda=lambda, rmse=rmse))
}

lambdal1 = getbestlambda(l1g, inputs, males$attr_o)
# lambda         rmse 
# 0.0008944587 0.7718279303 

lambdal2 = getbestlambda(l2g, inputs, males$attr_o)
# lambda      rmse 
# 0.0873895 0.7746840

sqrt(mean(stepwiseofficial$residuals ^ 2))
# rmse 0.7799603

# RMSE is lower for both regularized models

str(l1g$beta)
head(l1g$beta)

l1coeffs = l1g$beta[,match(lambdal1["lambda"], l1g$lambda)]
l2coeffs = l2g$beta[,match(lambdal2["lambda"], l2g$lambda)]

#            l1coeffs    l2coeffs  
# sinc_o    0.09360962  0.09744425
# intel_o  -0.05053763 -0.03745853
# fun_o     0.70110209  0.63793499
# amb_o    -0.05744147 -0.02489587
# sports    0.06366378  0.06110963
# tvsports -0.04119748 -0.03423351
# exercise  0.05442201  0.05347309
# dining    0.01124358  0.01521757
# museums   0.02513155  0.01104783
# art      -0.01987623 -0.01121441
# hiking   -0.01539261 -0.01505032
# gaming   -0.04861577 -0.04626823
# clubbing  0.01887559  0.01888717
# reading   0.00485301  0.00541505
# tv       -0.03554966 -0.03727099
# theater  -0.08516612 -0.07689660
# movies    0.01716196  0.01170390
# concerts -0.01221655 -0.01456787
# music     0.02953550  0.03533506
# shopping  0.02498822  0.02388139
# yoga      0.01166447  0.01012455

cbind(round(abs(l1coeffs) - abs(l2coeffs),3))
# for most part, l1s are higher magnitude.. exceptions are sinc_o, dining, reading, tv, concerts, music
#   and none of those are by muuch.
# fun is the biggest difference at 0.063... amb_o is 0.033
sd(l1coeffs) # 0.1592029
sd(l2coeffs) # 0.1443589
# "hiking" "concerts" "reading"  "dining"   "yoga" ... coefficients closest to zero
# occurs to me that the things close to zero might actually still be really important if the
# actual values of those variables tend toward extremes.





# Making cross-validated RMSE predictions
# -------------------------------------------------
df = read.csv("signal/speedDatingSimple.csv")
males = filter(df, gender == 1)
males = males[-(1:2)]

# -------------------------------------------------



subsetter = function(n, df) {
  # will pad with stuff
  indices = sample(1:nrow(df), nrow(df))
  indices = matrix(indices, ncol = n)
  lapply(1:n, function(coli) {
    return(df[indices[,coli],])
  })
  # return(list(subsample dfs))
}

subset10 = subsetter(10, males)

steptrainer = function(subsets, resultscolumn) {
  #str(Reduce(rbind, results[-2], data.frame()))
  
  results = vector(mode="list", length = length(subsets))

  for (i in seq(subsets)) {
    test = subsets[[i]]
    train = Reduce(rbind, subsets[-i], data.frame())
    # not sure.. double bracket? How to assign to lists without everything blowing up?
    step_model = step(lm(formula(paste(resultscolumn, "~ . -", resultscolumn)),
                         data=train),
                      scope=formula(paste(resultscolumn, "~ -."),
                         data=train),
                  direction="backward")

    predicted_vals = predict(step_model, test)
    results[[i]] = data.frame(y = test[[resultscolumn]],
                              stepy = predicted_vals)
  }
  all_examples = Reduce(rbind, results, data.frame())
  return(all_examples)
}


stepwise_results = steptrainer(subset10, "attr_o")

RMSE_step = sqrt(mean((stepwise_results$y - stepwise_results$stepy)^2)) # 0.8275177

# resultscolumn = "attr_o"
# train = males[1:100,]
# # works
# step(lm(formula("attr_o ~ sinc_o + intel_o + fun_o + amb_o"), data=males),
#      formula(attro_o ~ -., data=males),
#      direction="backward")
# # works
# summary(step(lm(formula("attr_o ~ sinc_o + intel_o + fun_o + amb_o"), data=males),
#      scope=formula("attro_o ~ -.", data=males),
#      direction="backward"))
# # works
# summary(step(lm(formula(paste("attr_o", "~ sinc_o + intel_o", "+ fun_o + amb_o")),
#                         data=males),
#              scope=formula("attro_o ~ -.", data=males),
#              direction="backward"))

input = as.matrix(males[-1])

l1fit = cv.glmnet(x=input, y=males[[1]], nfolds=10, alpha=1) #L1
l1fit$lambda.min # 0.06458715

l2fit = cv.glmnet(x=input, y=males[[1]], nfolds=10, alpha=0) #L2
l2fit$lambda.min # 0.105261


l1predictions = predict(l1fit, newx = input, s=l1fit$lambda.min)
l2predictions = predict(l2fit, newx = input, s=l2fit$lambda.min)
l1rmse = sqrt(mean((l1predictions - males[[1]])^2))
l2rmse = sqrt(mean((l2predictions - males[[1]])^2))
l1rmse #  0.8001598
l2rmse #  0.7757754     # best! Possibly also worth checking other measures.
# RMSEstep  0.8275177

l1coeffs = l1fit$glmnet.fit$beta[,match(l1fit$lambda.min, l1fit$glmnet.fit$lambda)]
l2coeffs = l2fit$glmnet.fit$beta[,match(l2fit$lambda.min, l2fit$glmnet.fit$lambda)]
cbind(l1coeffs, l2coeffs)

# soooo biggest difference... l1 gets rid of some parameters entirely.
# its hella more interpretable
#           l1coeffs     l2coeffs
# sinc_o    0.00000000  0.097361754 # 2nd highest l2coefficient, but missing in l1
# intel_o   0.00000000 -0.033973065
# fun_o     0.67512700  0.626660934
# amb_o     0.00000000 -0.019303289
# sports    0.02948493  0.060558799
# tvsports  0.00000000 -0.032984859
# exercise  0.04135678  0.053274516
# dining    0.00000000  0.015754266
# museums   0.00000000  0.008988976
# art       0.00000000 -0.010015076
# hiking    0.00000000 -0.014896262
# gaming   -0.02688290 -0.045757528
# clubbing  0.00000000  0.018862356
# reading   0.00000000  0.005353439
# tv       -0.02146858 -0.037357643
# theater  -0.04021369 -0.075336180
# movies    0.00000000  0.010579795
# concerts  0.00000000 -0.014623431
# music     0.00000000  0.035986067
# shopping  0.00000000  0.023565286
# yoga      0.00000000  0.009788307

# nice correlation, coerced at l2~0
ggplot(data.frame(l1coeffs[-3], l2coeffs[-3]), aes(y=l1coeffs[-3], x=l2coeffs[-3])) + geom_point() + geom_smooth(method=lm)

# L1 wow such correlate
ggplot(data.frame(y = males[[1]], l1 = l1predictions)) + geom_point(aes(x=l1predictions, y=y))

# also wow
ggplot(data.frame(y = males[[1]], l2 = l2predictions)) + geom_point(aes(x=l2predictions, y=y))

# Elastic net regression
# -----------------------------
df = read.csv("signal/speedDatingSimple.csv")
males = filter(df, gender == 1)
males = males[-(1:2)]
# -----------------------------
# install.packages("caret")
library("caret")

input = as.matrix(males[-1])
yvals = males[[1]]

fitted = cv.glmnet(x=input, y=yvals, nfolds=10, alpha=0)
fitted2 = cv.glmnet(x=input, y=yvals, nfolds=10, alpha=1)

all_lambdas = cbind(c(fitted$lambda, fitted2$lambda),
                    c(fitted$cvm, fitted2$cvm))

all_lambdas = all_lambdas[order(all_lambdas[,2]),]


param_grid = expand.grid(.alpha = 1:10 * 0.1,
                         .lambda = seq(0, 0.25, length.out=50))
control = trainControl(method="repeatedcv", number=2,
                       repeats=3, verboseIter=TRUE)
caret_fit = train(x=input, y=yvals, method="glmnet",
                  tuneGrid=expand.grid(.alpha = 1:2, .lambda = 1:2), trControl=control)

# Fitting alpha = 1, lambda = 0.0306 on full training set

class(caret_fit$finalModel)
predictions = predict(caret_fit)
sqrt(mean((predictions - yvals)^2))
# 0.7822741


RMSE_calculator = function(wholedf, filter_gender, rating) {
  filtereddf = filter(wholedf, gender == filter_gender)[-(1:2)]
  yvals = filtereddf[[rating]]
  inputs = as.matrix(filtereddf[-match(rating, names(filtereddf))])
  
  fitted = cv.glmnet(x=inputs, y=yvals, nfolds=10, alpha=0)
  fitted2 = cv.glmnet(x=inputs, y=yvals, nfolds=10, alpha=1)
  
  all_lambdas = cbind(c(fitted$lambda, fitted2$lambda),
                      c(fitted$cvm, fitted2$cvm))
  all_lambdas = all_lambdas[order(all_lambdas[,2]),]
  lambdas = range(all_lambdas[1:10,1])
  lambdas = seq(lambdas[1],lambdas[2], length.out=20)
  
  param_grid = expand.grid(.alpha = 1:10 * 0.1,
                           .lambda = lambdas)
  control = trainControl(method="repeatedcv", number=10,
                         repeats=3, verboseIter=TRUE)
  caret_fit = train(x=inputs, y=yvals, method="glmnet",
                    tuneGrid=param_grid, trControl=control)
  
  predictions = predict(caret_fit)
  returnsults = c(RMSE = sqrt(mean((predictions - yvals)^2)),
                         alpha = unlist(caret_fit$bestTune["alpha"]),
                         lambda = unlist(caret_fit$bestTune["lambda"]))
  return(returnsults)
}


results = matrix(vector(mode="numeric",length=30), ncol=3)
colnames(results) = c("RMSE", "alpha", "lambda")
rnames = vector(mode="character", length=10)

i = 1
for (gender in c(0, 1)) {
  for (rating in c("attr_o", "sinc_o", "fun_o", "amb_o", "intel_o")) {
    results[i,] = RMSE_calculator(df, gender, rating)
    rnames[i] = paste("g", gender, rating)
    i = i + 1
  }
  # rowname will be (gender[], rating[])
}

rownames(results) = rnames




# aaagh we forgot to scale input matrices. I wonder if that makes a difference
# also it apparently helps to modify the range of lambda used in glmnet()
# if the optimal value is at the extreme end. Makes sense.


temp = RMSE_calculator(df, 1, "amb_o")
 # before scale == rmse 0.5558797, alpha 0.9,  lambda 0.033944250
 # try2 == 0.55760358    0.90000000    0.03864096 
