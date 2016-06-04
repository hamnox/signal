library("ggplot2")
# returns x and y between 0 and 1,
# where y > m*x + b if label = 1
# and   y < m*x + b if label = -1
lin_pair = function(m, b, label) {
  if (!(label %in% c(-1, 1))) {
    stop("invalid label")
  }
  i = 0
  while(TRUE) {
    i = i + 1
    if (i > 1000) {
      stop("something went wrong")
    }
    x = runif(1)
    threshold = m * x + b
    if (label == 1) {
      if (threshold >= 1) {
        next
      }
      y = runif(1, min=max(0, threshold), max=1)
      break
    } else {
      if (threshold <= 0) {
        next
      }
      y = runif(1, min=0, max=min(1, threshold))
      break
    }
  }
  return(c(x=x, y=y))
}
quad_pair = function(a, b, c, label) {
  if (!(label %in% c(-1, 1))) {
    stop("invalid label")
  }
  i = 0
  while(TRUE) {
    i = i + 1 
    if (i > 1000) {
      stop("something went wrong")
    }
    x = runif(1)
    threshold = a * (x - b)^2 + c
    if (label == 1) {
      if (threshold >= 1) {
        next
      }
      y = runif(1, min=max(0, threshold), max=1)
      break
    } else {
      if (threshold <= 0) {
        next
      }
      y = runif(1, min=0, max=min(1, threshold))
      break
    }
  }
  return(c(x=x, y=y))
}

library("MASS")
# return a point sampled from a multivariate normal distribution
mvnorm_pair = function(mu, cov) {
  return(mvrnorm(1, mu, cov))
}
# in a p-dimensional space, for our two classes of data,
# we have to estimate sum(p + p:1) parameters (means + covariances)


# generate 100 data points from two different multivariate distributions with the same covariances
resultsv = matrix(rep(NA_real_, 200), ncol=2)
i = 1
while(i <= 50) {
  point = mvnorm_pair(c(0.25, 0.25), matrix(c(0.2, 0.1, 0.1, 0.2), nrow=2))
  if (sum(point > 1) + sum(point < 0) == 0) {
    resultsv[i,] = point
    i = i + 1
  }
}
while(i <= 100) {
  point = mvnorm_pair(c(0.75, 0.75), matrix(c(0.2, 0.1, 0.1, 0.2), nrow=2))
  if (sum(point > 1) + sum(point < 0) == 0) {
    resultsv[i,] = point
    i = i + 1
  }
}

# plot the generated data
qplot(resultsv[,1], resultsv[,2])

# label with appropriate class
vlabeled = c(rep(1,50), rep(-1, 50))

# run LDA and QDA, compare estimates
lda1 = MASS::lda(resultsv, grouping = factor(vlabeled))
# estimates first group mean is .425, .393 (actual is .25, .25)
# estimates second group mean is .586, .573 (actual is .75, .75)
# coefficients estimated -2.72, -2.58
# no idea what relation that has to the covariance I put in

qda1 = MASS::qda(resultsv, grouping = factor(vlabeled))
# estimates the first group mean is .425, .393
# estimates the second group mean is 0.586, 0.573
# covariance? estimated for first group is (3.65, 0, 0.415, -3.719)
# covariance? estimated for second group is (-4.95, 0, 0.738, -3.986)
# still don't understand how this relates to the covariance I put in

# check accuracy
library(pROC)
p_lda1 = as.data.frame(predict(lda1, resultsv))
p_qda1 = as.data.frame(predict(qda1, resultsv))
# check whether p2 = 1 - p1
( ggplot(data.frame()) + geom_point(aes(x=p_lda1$class, y= round(1 - p_lda1$posterior..1,5) - round(p_lda1$posterior.1),5), color="blue", alpha=0.3))

# plot against each other
( ggplot(data.frame(), aes(color=vlabeled)) + geom_point(aes(x=p_lda1$posterior..1, y=p_qda1$posterior..1), shape=1, alpha=0.8) + geom_point(aes(x=p_lda1$posterior.1, y=p_qda1$posterior.1), shape=2, alpha=0.8) )

# auc
roc(vlabeled, p_lda1$posterior.1, plot=TRUE) # .736
roc(vlabeled, p_qda1$posterior.1, plot=TRUE) # .788


partimat(as.data.frame(resultsv), grouping=factor(vlabeled), method="lda")
partimat(as.data.frame(resultsv), grouping=factor(vlabeled), method="qda")


# ------------------------------------------------
# generate 100 data points from two different multivariate
# distributions with different covariance matrices
# ------------------------------------------------
twov = matrix(rep(NA_real_, 200), ncol=2)
i = 1
while(i <= 50) {
  point = mvnorm_pair(c(0.25, 0.25), matrix(c(0.9, 0.1, 0.1, 0.9), nrow=2))
  if (sum(point > 1) + sum(point < 0) == 0) {
    twov[i,] = point
    i = i + 1
  }
}
while(i <= 100) {
  point = mvnorm_pair(c(0.75, 0.75), matrix(c(0.2, 0.19, 0.19, 0.2), nrow=2))
  if (sum(point > 1) + sum(point < 0) == 0) {
    twov[i,] = point
    i = i + 1
  }
}
# plot the data
qplot(twov[1:50,1], twov[1:50,2])
qplot(twov[51:100,1], twov[51:100,2])


# cutting off results outside the box is probably throwing off the means
lda2 = MASS::lda(twov, grouping = factor(vlabeled))
# estimated first group mean is .457, .497 (actual is .25, .25)
# estimated second group mean is .521, .508 (actual is .75, .75)
# coefficients estimated -3.874, 0.925

qda2 = MASS::qda(twov, grouping = factor(vlabeled))
# estimated first group mean is .457, .497
# estimated second group mean is .521, .508
# scaling first group is (3.41, 0, -0.123, 3.157)
# scaling second group is (-3.83, 0, -8.07, 9.19)

# get the AUC
p_lda2 = as.data.frame(predict(lda2, twov))
p_qda2 = as.data.frame(predict(qda2, twov))
roc(vlabeled, p_lda2$posterior.1, plot=TRUE) # .559
roc(vlabeled, p_qda2$posterior.1, plot=TRUE) # .878


partimat(as.data.frame(twov), grouping=factor(vlabeled), method="lda")
partimat(as.data.frame(twov), grouping=factor(vlabeled), method="qda")


# --------------------------------------------
# generate much datas
# --------------------------------------------
muchdata = matrix(rep(NA_real_, 400), ncol=2)
for (i in 1:100) {
  muchdata[i,] = lin_pair(1.5, 0.2, 1)
}
for (i in 101:200) {
  muchdata[i,] = lin_pair(1.5,.05, -1)
}
mlabels = c(rep(1,100), rep(-1,100))

# plot the data
qplot(muchdata[,1], muchdata[,2])
qplot(muchdata[1:100, 1], muchdata[1:100,2])
qplot(muchdata[101:200, 1], muchdata[101:200,2])

m1lda = lda(muchdata, grouping = factor(mlabels))
m1qda = qda(muchdata, grouping = factor(mlabels))
# above group means is x=0.258, y=0.786
# below group means is x=0.498, y=0.352
# lda coefficients are -4.01, 5.13
# qda first group is (6.72, 0, -4.5, 7.17)
# qda second group is (-3.43, 0, -2.17, 4.56)

# HOW DOES PERFORMANCE DIFFERS!?!? I DOHNT NO
cov(muchdata[1:100,])
# 0.0222 0.0139
# 0.0139 0.0282
cov(muchdata[101:200,])
# 0.0850 0.0405
# 0.0405 0.0673

# get the AUC

p_m1l = as.data.frame(predict(m1lda, muchdata))
p_m1q = as.data.frame(predict(m1qda, muchdata))
roc(mlabels, p_m1l$posterior.1, plot=TRUE) # .996
roc(mlabels, p_m1q$posterior.1, plot=TRUE) # .998

partimat(as.data.frame(muchdata), grouping=factor(mlabels), method="lda")
partimat(as.data.frame(muchdata), grouping=factor(mlabels), method="qda")

# --------------------------------------------
# generate much mucher datas
# --------------------------------------------
mucherdata = matrix(rep(NA_real_, 40000), ncol=2)
for (i in 1:10000) {
  mucherdata[i,] = lin_pair(1.5, 0.2, 1)
}
for (i in 10001:20000) {
  mucherdata[i,] = lin_pair(1.5,.05, -1)
}
merlabels = c(rep(1,10000), rep(-1,10000))
# qplot(mucherdata[1:10000, 1], mucherdata[1:10000,2])
# qplot(mucherdata[10001:20000, 1], mucherdata[10001:20000,2])

m2lda = lda(mucherdata, grouping = factor(merlabels))
m2qda = qda(mucherdata, grouping = factor(merlabels))

# get the AUC
p_m2l = as.data.frame(predict(m2lda, mucherdata))
p_m2q = as.data.frame(predict(m2qda, mucherdata))
roc(merlabels, p_m2l$posterior.1, plot=TRUE) # .997
roc(merlabels, p_m2q$posterior.1, plot=TRUE) # .998
# above means: 0.266 0.801
# below means: 0.499 0.348

partimat(as.data.frame(mucherdata), grouping=factor(merlabels), method="lda")
partimat(as.data.frame(mucherdata), grouping=factor(merlabels), method="qda")

# --------------------------------------------
# generate much lesser datas
# --------------------------------------------
lesserdata = matrix(rep(NA_real_, 120), ncol=2)
for (i in 1:30) {
  lesserdata[i,] = lin_pair(1.5, 0.2, 1)
}
for (i in 31:60) {
  lesserdata[i,] = lin_pair(1.5,.05, -1)
}
lerlabels = c(rep(1,30), rep(-1,30))
qplot(lesserdata[1:30, 1], lesserdata[1:30,2])
qplot(lesserdata[31:60, 1], lesserdata[31:60,2])

l2lda = lda(lesserdata, grouping = factor(lerlabels))
l2qda = qda(lesserdata, grouping = factor(lerlabels))

# get the AUC
p_l2l = as.data.frame(predict(l2lda, lesserdata))
p_l2q = as.data.frame(predict(l2qda, lesserdata))
roc(lerlabels, p_l2l$posterior.1, plot=TRUE) # 1
roc(lerlabels, p_l2q$posterior.1, plot=TRUE) # 1
# above means: 0.266 0.801
# below means: 0.499 0.348

# visualize stuff
partimat(as.data.frame(lesserdata), grouping=factor(lerlabels), method="lda")
partimat(as.data.frame(lesserdata), grouping=factor(lerlabels), method="qda")


# --------------------------------------------
# Analyze the speed-dating dataset
# --------------------------------------------
spd_df = read.csv("datasets/speeddating-aggregated.csv")
spd_df = dplyr::select(spd_df, gender, sports:yoga)

g1_df = filter(spd_df, gender == 0)
colMeans(g1_df)
#   gender   sports tvsports exercise   dining  museums      art   hiking   gaming 
# 0.00     5.73     4.12     6.37     8.14     7.44     7.21     5.94     3.25 
# clubbing  reading       tv  theater   movies concerts    music shopping     yoga 
# 5.91     7.90     5.72     7.50     8.14     7.15     8.04     6.47     5.06

g2_df = filter(spd_df, gender == 1)
colMeans(g2_df)
#   gender   sports tvsports exercise   dining  museums      art   hiking   gaming clubbing  reading 
# 1.00     7.05     4.97     6.20     7.41     6.51     6.17     5.58     4.43     5.59     7.39 
# tv  theater   movies concerts    music shopping     yoga 
# 4.93     6.03     7.66     6.54     7.71     4.75     3.77 

# male has a higher average sports, gaming
# female has higher average dining and museums and art, reading, shopping, yoga..
  # female is just higher in lots of things

# compare covariances
library("corrplot")
corrplot(cov(g1_df), type="lower", is.corr=FALSE)
corrplot(cov(g2_df), type="lower", is.corr=FALSE)
# pretty similar but not quite the same. probably qdf will work better
# qdf almost always works better.

# get different sized samples of dataset
rsets = vector(mode="list", 8)
names(rsets) = c(.01, .02, .05, .1, .25, .5, .75, 1)
for (percent in c(.01, .02, .05, .1, .25, .5, .75, 1)) {
  count = round(nrow(spd_df) * percent)
  idx = sample(1:nrow(spd_df), count)
  rsets[[as.character(percent)]] = spd_df[idx,]
}


magic = matrix(rep(NA, 8*5), ncol=5)
magic[,1] = c(.01, .02, .05, .1, .25, .5, .75, 1)
rownames(magic) = names(rsets)
colnames(magic) = c("sampleN", "ldaset", "ldaall", "qdaset", "qdaall")
i = 1
for (set in rsets) {
  thelda = MASS::lda(set[,-1], grouping=factor(set[[1]]))
  thelda_p = predict(thelda, set[,-1])
  thelda_allp = predict(thelda, spd_df[,-1])
  print(i)
  warnings()
  if (i > 3) {
    theqda = MASS::qda(set[,-1], grouping=factor(set[[1]]))
    theqda_p = predict(theqda, set[,-1])
    theqda_allp = predict(theqda, spd_df[,-1])
    magic[i,2:5] = c(roc(set[[1]], thelda_p$posterior[,1])$auc,
                  roc(spd_df[[1]], thelda_allp$posterior[,1])$auc,
                  roc(set[[1]], theqda_p$posterior[,1])$auc,
                  roc(spd_df[[1]], theqda_allp$posterior[,1])$auc)
  } else {
    magic[i,2:5] = c(roc(set[[1]], thelda_p$posterior[,1])$auc,
                  roc(spd_df[[1]], thelda_allp$posterior[,1])$auc,
                  0,0)
  }
  i = i + 1
}
magic_df = tidyr::gather(as.data.frame(magic), "type", "auc", ldaset:qdaall)
ggplot(magic_df, aes(x=sampleN, y=auc, color=type, shape=type)) + geom_point(alpha="0.5", size=5)
# in conclusion, qdaf is really bad at generalizing from small sample sizes.
# around 0.3 is about where the tradeoff starts looking good

install.packages("rattle")
library("rattle")
data(wine, package='rattle')
df_wine = wine

# multivariate prediction
wine_lda = MASS::lda(Type ~ ., data=df_wine)
# group means
# Alcohol Malic  Ash Alcalinity Magnesium Phenols Flavanoids Nonflavanoids
# 1    13.7  2.01 2.46       17.0     106.3    2.84      2.982         0.290
# 2    12.3  1.93 2.24       20.2      94.5    2.26      2.081         0.364
# 3    13.2  3.33 2.44       21.4      99.3    1.68      0.781         0.448
# Proanthocyanins Color   Hue Dilution Proline
# 1            1.90  5.53 1.062     3.16    1116
# 2            1.63  3.09 1.056     2.79     520
# 3            1.15  7.40 0.683     1.68     630
# Coefficients of linear discriminants:
#                    LD1       LD2
# Alcohol         -0.40340  0.871793
# Malic            0.16525  0.305380
# Ash             -0.36908  2.345850
# Alcalinity       0.15480 -0.146381
# Magnesium       -0.00216 -0.000463
# Phenols          0.61805 -0.032213
# Flavanoids      -1.66119 -0.491998
# Nonflavanoids   -1.49582 -1.630954
# Proanthocyanins  0.13409 -0.307088
# Color            0.35506  0.253231
# Hue             -0.81804 -1.515634
# Dilution        -1.15756  0.051184
# Proline         -0.00269  0.002853

# actually pretty hard to interpret since not scaled.
# biggest factors between 1 and 2 likely flavanoids, nonflavanoids, dilution, maybe proline adds up pretty high
# biggest diff between 1 and 3 likely Ash, Nonflavanoids, Hue

wine_pred = predict(wine_lda)
plot(wine_lda)

# plot values of two discriminant functions against each other
MASS::ldahist(wine_pred$x[,1], g = df_wine$Type)
# group 1 and 2 harder to tell apart here

MASS::ldahist(wine_pred$x[,2], g = df_wine$Type)
# group 1 and 3 not differentiated at all on this metric




# ------------------------------
# perceptrons
# ------------------------------
perceptdata = matrix(rep(NA_real_, 1000*2*3), ncol=3)
names(perceptdata) = c("intercept","x", "y")
for (i in 1:1000) {
  perceptdata[i,] = c(1,lin_pair(1.5, 0.2, 1))
}
for (i in 1001:2000) {
  perceptdata[i,] = c(1,lin_pair(1.5,.05, -1))
}

# create labels vector
labels = c(rep(1,1000), rep(-1, 1000))

# check for errors
sum(is.na(perceptdata))

# verify that data is linearly separable
qplot(perceptdata[,2], perceptdata[,3])

# input matrix xs, label vector y, weights vector w, learning rate float rate
# iterations integer niter
perceptron = function(xs, y, w, rate, niter=nrow(xs)) {
  indices = sample(1:nrow(xs), niter, replace = FALSE)
  weights = w
  for (i in indices) {
    xi = xs[i,]
    yhat = sign( sum(xi * w) )
    # if it's a false negative, add to weight
    if (yhat <= 0 & y[i] == 1) {
      weights = weights + rate * xi
      # print(paste0('+i ', i, " yhat ", yhat, " y ", y[i]))
    } else {
      # if it's a false positive, subtract from weight
      if (yhat > 0 & y[i] == -1)
      weights = weights - rate * xi
      # print(paste0('-i ', i, " yhat ", yhat, " y ", y[i]))
    }
  }
  return (weights)
}

silenced = perceptron(perceptdata, labels, rep(0, ncol(perceptdata)), 1)

set.seed(5)
silenced = perceptron(perceptdata, labels, silenced, .05)
silenced
# plot the decision boundary with geom_abline if have time
# ????
# qplot(perceptdata[,2], perceptdata[,3]) + geom_abline(intercept=.5, slope = -1 * (silenced[3] - silenced[1])/silenced[2], color="red")

dot_prod = function(x, w) {
  # iterate over rows
  sapply(seq_len(nrow(x)), function(n) {
    row = x[n,]
    return(sum(row * w))
  })
}

perceptron_conv = function(xs, y, w, rate, maxepochs=1000) {
  thisreturn = w
  i = 0
  while( i <= maxepochs ) {
    thisreturn = perceptron(xs, y, thisreturn, rate, niter=nrow(xs)/2)
    i = i + 1

    # get predictions
    yhats = dot_prod(xs, thisreturn)
    yhats = sign(yhats)
    # fix 0s to be negative
    yhats[yhats==0] = -1
    match = sum(yhats == y)
    print(paste0("epoch ", i, " match ", match, " weights:"))
    print(thisreturn)
    if (match == 0) {
      return(thisreturn)
    }
  }
  stop("out of iterations")
}

set.seed(5)
# can't make it converge. NOOOOOOOOO
perceptron_conv(perceptdata, labels,
                runif(3), .1)

# -----------------------------------
# svms
# -----------------------------------