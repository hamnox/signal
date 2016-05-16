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
alldf %>% select(Extraversion, Neuroticism, active:scornful) -> df

# fill NAs with means
colmeans = sapply(colnames(df), function(column) {
  nas = is.na(df[[column]])
  lemean = mean(df[!nas,column])
  df[nas,column] <<- lemean
  return(lemean)
})
df %>% select(active:scornful) -> features
df %>% select(Extraversion) -> EV
df %>% select(Neuroticism) -> NC

# elastic net regress: alpha, lambda
# n-fold cross-validation
alphas = seq(0,1,.1)
lambdas = 10^seq(1,-3,length.out=50) ###argh I want to have an lambda of 0 too.

# fold assignments
set.seed(1)
nfold = 10
fold_indexes = (sample(1:nrow(df), nrow(df)) %% nfold) + 1
# train and test sets


# this creates the datasets, a list of 1:10 folds with trainx, testx,
  # and sublists trainy, testy with NC and EV as extraversion and neuroticism
# all those results are scaled matrices
# also returns a scaleX_fn

data_sets = lapply(1:nfold, function(n) {
  trainx = scale(as.matrix(features[fold_indexes == n,]))
  trainy_NC = scale(as.matrix(df[fold_indexes == n,2]))
  trainy_EV = scale(as.matrix(df[fold_indexes == n,1]))
  
  scaleX = function(x) {
    return (scale(x, center = attr(trainx, 'scaled:center'),
          scale = attr(trainx, 'scaled:scale')))
  }
  testx = scaleX(as.matrix(features[fold_indexes != n,]))
  testy_NC = scale(as.matrix(df[fold_indexes != n,2]),
                center = attr(trainy_NC, 'scaled:center'),
                scale = attr(trainy_NC, 'scaled:scale'))
  testy_EV = scale(as.matrix(df[fold_indexes != n,1]),
                   center = attr(trainy_EV, 'scaled:center'),
                   scale = attr(trainy_EV, 'scaled:scale'))

  return(list(trainx = trainx,
              testx = testx,
              trainys = list(
                NC = trainy_NC,
                EV = trainy_EV
                ),
              testys = list(
                NC = testy_NC,
                EV = testy_EV
                ),
              scaleX_fn = scaleX
            ))
})
# this is to save time later, since the predictions are always generated
# in the same order
data_sets[[nfold+1]] = unlist(sapply(1:nfold, function(i) {
                  data_sets[[i]][["testys"]][["NC"]]
                }))
data_sets[[nfold+2]] = unlist(sapply(1:nfold, function(i) {
  data_sets[[i]][["testys"]][["EV"]]
}))

names(data_sets) = c(1:10, "alltestyNC", "alltestyEV")

# reference:
# data_sets(1(trainx, testx, trainys(NC, EV), testys(NC, EV), scaleX_fn)...)


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


# generates




# more convenient convenience fn
cross_validated_rmse = function (current_alpha, fits, allys) {
  # all of the neurot_ys in the expected order. don't want to
  # calculate every time this in the loop below, because it'll stay the same
  return(cbind(current_alpha,
    t(sapply(lambdas, function(current_lambda) {
    print(paste("Trying lambda", current_lambda))
    
    # this is a complex little beauty that generates the predictions
    # data_sets(1(trainx, testx, trainys(NC, EV), testys(NC, EV), scaleX_fn)...)
    yhats = do.call(rbind,sapply(1:nfold, function(i) {
      predict(fits[[i]],
              data_sets[[i]][["testx"]],
              s=current_lambda)
    }))
    if (length(yhats) != length(allys)) {
      print("You've got a problem: your yhats dont match your ys")
    }
    thermse = rmse(yhats, allys)
    return(c(current_lambda, thermse))})
  )))
}

cross_by_alpha = function (alphas, lambdas, data_sets) {
  do.call(rbind,
      sapply(alphas, function(current_alpha) {
      print(paste("Trying alpha", current_alpha))
      neurot_fits = lapply(data_sets[1:nfold], function(fold) {
        glmnet(fold[["trainx"]],
               fold[["trainys"]][["NC"]],
               alpha = current_alpha,
               lambda = lambdas)
      })
      extrav_fits = lapply(data_sets[1:nfold], function(fold) {
        glmnet(fold[["trainx"]],
               fold[["trainys"]][["EV"]],
               alpha = current_alpha,
               lambda = lambdas)
      })
      
      neurot_rmses = cross_validated_rmse(current_alpha,
                                          neurot_fits,
                                          data_sets$alltestyNC)
      extrav_rmses = cross_validated_rmse(current_alpha,
                                          neurot_fits,
                                          data_sets$alltestyEV)
      colnames(neurot_rmses) = c("alpha", "lambda", "neuroticism_rmse")
      colnames(extrav_rmses) = c("alpha", "lambda", "extraversion_rmse")
      rmses = inner_join(as.data.frame(neurot_rmses),
                         as.data.frame(extrav_rmses),
                         by = c("alpha", "lambda"))
      return(rmses)
  }))
}


results = cross_by_alpha(alphas, lambdas, data_sets) #... well that didn't work


# hashmap collisions
-------------------------
hashmapping = t(sapply(1:1000, function(iteration) {
  placement = sample(1:10, 10, replace=TRUE)
  collisions = sum(sapply(1:10, function(i) {
    assigned = sum(placement==i)
    if (assigned > 1) {
      return(assigned-1)
    } else { return(0)}
  }))
  unused_count = sum(sapply(1:10, function(i) {
    sum(placement==i) == 0
  }))
  was_collided = collisions > 0
  return(c(collisions, unused_count, was_collided))
}))
colnames(hashmapping) = c("collisions", "unused_count", "was_collided")

# collision_probability 100...
sum(hashmapping[,"was_collided"])/1000

# expect number of hash collisions 3.499
mean(hashmapping[,"collisions"])

# expected number of unused hashes 3.499
mean(hashmapping[,"unused_count"])


