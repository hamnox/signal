# see redo on day 11
library("dplyr")
library("ggplot2")
library("glmnet")
library("psych")
library("corrplot")


# PART 1: REGULARIZATION
#-------------------------

# load msq data set
alldf = msq
df <- select(alldf, Extraversion, Neuroticism, active:scornful)

# fill NAs with means
# colmeans = sapply(colnames(df), function(column) {
#   nas = is.na(df[[column]])
#   lemean = mean(df[!nas,column])
#   df[nas,column] <<- lemean
#   return(lemean)
# })

# NAs in data set
sum(is.na(df)) # 16300

# replace NAs with column means
for (i in seq(df)) {
  nas = is.na(df[[i]])
  mean = mean(df[!nas, i])
  df[nas,i] = mean
}

# check NAs
sum(is.na(df)) # 16300


# dataframes
features = select(df, active:scornful)
EV = select(df, Extraversion)
NC = select(df, Neuroticism)

# fold assignments
set.seed(1)
nfold = 10
fold_indexes = (sample(nrow(df)) %% nfold) + 1

# train and test sets

# this creates the datasets, a list of 1:10 folds with trainx, testx,
  # and sublists trainy, testy with NC and EV as extraversion and neuroticism
# all those results are scaled matrices
# also returns a scaleX_fn

train_features = vector(mode="list", length=nfold)
test_features = vector(mode="list", length=nfold)
train_targets = list(NC = vector(mode="list", length=nfold), EV = vector(mode="list", length=nfold))
test_targets = list(NC = vector(mode="list", length=nfold), EV = vector(mode="list", length=nfold))

for (n in 1:nfold) {
  # get training folds
  train_features[[n]] = scale(features[fold_indexes != n,])
  train_targets$NC[[n]] = NC[fold_indexes != n,]
  train_targets$EV[[n]] = EV[fold_indexes != n,]
  
  # get test folds
  test_features[[n]] = scale(features[fold_indexes == n,])
  test_targets$NC[[n]] = NC[fold_indexes == n,]
  test_targets$EV[[n]] = EV[fold_indexes == n,]
}

# n-fold cross validation:
# end results of data frame ought to be
# alpha, lambda, Extraversion RMSE and Neuroticism RMSE

# rmse convenience function
rmse = function(y, yhat) {
  sqrt(mean((y - yhat)^2))
}

# TESTING SOME THEORIES
silenced = data.frame(a=c(1,2,3,4), b=c(5,6,7,8), c = c(9,10,11,12))
silenced[c(2,4), c("a", "b")] = data.frame(c(100,200), c(300,400))
silenced # IT WORKS you can subset a data frame in weird square ways


# more convenient convenience fn

# elastic net regress: alpha, lambda options
alphas = seq(0,1,.1)
lambdas = 10^seq(1,-3,length.out=50) ###argh I want to have an lambda of 0 too.

# assign the memory
all_rmses = expand.grid(alpha = alphas, lambda = lambdas, fold = seq_len(nfold))
all_rmses$EV_rmse = NA_integer_
all_rmses$NC_rmse = NA_integer_

# here's where the magic happens
for (current_alpha in alphas) {
  print(paste("alpha"), current_alpha)
  # the nice thing about assigning in a for loop instead of a function
  # is that it STAYS ASSIGNED
  alpha_NCfits = vector(mode="list", length=nfold)
  alpha_EVfits = vector(mode="list", length=nfold)
  for (n in 1:nfold) {
    print(paste("nfold", n, "alpha", current_alpha))
    alpha_NCfits[[n]] = glmnet(train_features[[n]], train_targets$NC[[n]],
                               alpha = current_alpha,
                               lambda = lambdas)
    alpha_EVfits[[n]] = glmnet(train_features[[n]], train_targets$EV[[n]],
                               alpha = current_alpha,
                               lambda = lambdas)
    
    NC_predictions = predict(alpha_NCfits[[n]], newx = test_features[[n]], s = lambdas)
    EV_predictions = predict(alpha_EVfits[[n]], newx = test_features[[n]], s = lambdas)
    
    # print("....collecting rmses")
    for (i in seq(lambdas)) {
      NC_rmse = rmse(test_targets$NC[[n]], NC_predictions[,i])
      EV_rmse = rmse(test_targets$EV[[n]], EV_predictions[,i])
      rmse_rowindex = all_rmses$alpha == current_alpha & all_rmses$lambda == lambdas[i] & all_rmses$fold == n
      if (sum(rmse_rowindex) != 1) {
        print(paste("problem encountered: row_index =",rmse_rowindex))
      }
      all_rmses[rmse_rowindex,]$NC_rmse = NC_rmse
      all_rmses[rmse_rowindex,]$EV_rmse = EV_rmse
    }
  }
}

View(all_rmses)

# get the best rmses
minNC_rmse = min(all_rmses$NC_rmse)
minEV_rmse = min(all_rmses$EV_rmse)

# get the rows with the best rmse
bestNC = all_rmses[all_rmses$NC_rmse == minNC_rmse,]
bestEV = all_rmses[all_rmses$EV_rmse == minEV_rmse,]

# sort by lambda in case there are multiple options with the same RMSE
# take only the first row
bestNC = bestNC[order(bestNC$lambda, decreasing=TRUE),][1,]
bestEV = bestEV[order(bestEV$lambda, decreasing=TRUE),][1,]

# fitting
NCfit = glmnet(scale(features), NC$Neuroticism, alpha = bestNC[["alpha"]], lambda = bestNC[["lambda"]])
EVfit = glmnet(scale(features), EV$Extraversion, alpha = bestEV[["alpha"]], lambda = bestEV[["lambda"]])

NCcoef = coef(NCfit)
EVcoef = coef(EVfit)
plottify = data.frame(NC=NCcoef[-1,1], EV=EVcoef[-1,1], row.names = rownames(NCcoef)[-1])

quantileNC = quantile(abs(plottify[, "NC"]))
quantileEV = quantile(abs(plottify[, "EV"]))
#  NC
#         0%        25%        50%        75%       100% 
# 0.00000000 0.00000000 0.02245625 0.10045799 0.80107472 

#  EV
#          0%         25%         50%         75%        100% 
# 0.000000000 0.006432372 0.063836546 0.150855550 0.631753591 

goodvars = rownames(plottify)[plottify$NC > quantileNC[3] | plottify$EV > quantileEV[3]]
corrplot(as.matrix(plottify[goodvars,]), is.corr=FALSE, cl.pos="n")


# hashmap collisions
# -------------------------
iterations = 1000

hashmapping = matrix(vector(mode="numeric", length = iterations * 3), nrow = 1000)
colnames(hashmapping) = c("collisions", "unused_count", "was_collided")

for (iter in seq_len(iterations)) {
  placement = sample(1:10, 10, replace=TRUE)
  # record collisions
  hashmapping[iter, "collisions"] = sum(sapply(1:10, function(i) {
                                              assigned = sum(placement==i)
                                              if (assigned > 1) {
                                                return(assigned-1)
                                              } else { return(0) }
                                           }))
  # record unused
  hashmapping[iter, "unused_count"] = sum(sapply(1:10, function(i) {
                                                          sum(placement==i) == 0
                                                        }))
  hashmapping[iter, "was_collided"] = hashmapping[iter, "collisions"] > 0
}




# collision_probability 100...
sum(hashmapping[,"was_collided"])/1000

# expect number of hash collisions 3.499
mean(hashmapping[,"collisions"])

# expected number of unused hashes 3.499
mean(hashmapping[,"unused_count"])



# Rolling the dice
# -------------------------

# simulate the game 1000 times
rolldice = function(n) {
  sapply(1:n, function(iteration) {
    values = c()
    cnt = 0
    # keep rolling until you collect them all
    while(length(values) < 6) {
      roll = sample(6,1)
      values[as.character(roll)] = 1
      cnt = cnt + 1
    }
    return(cnt)
  })
}
meh = rolldice
summary(meh)
   # Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   # 6.00   10.00   13.00   14.56   17.00   55.00
# expect to have 14.5 rolls

# try to get a variance on this number
trials_of_trials = sapply(1:100, function(iteration) {
  mean(rolldice(1000))
})

summary(trials_of_trials)
  #  Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  # 14.20   14.54   14.73   14.70   14.83   15.32

# HAVE NOT DONE AMOEBA PROBLEM