
# # data loading
# library("foreign")
library("psych")
# fitting
library("glmnet")

# dataframe manipulation
library("dplyr")
library("dummies")

# plotting libraries
library("corrplot")
library("pROC")
library("GGally")

(function(notes){
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # notes
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  cat(paste(
    "",
    sep = "\n"))
})()

(function(miscwork) {
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # misc work
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
})


# --------------------------------------------------------------------
# --------------------------------------------------------------------
# --------------------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# initialization
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# clean data grabs, no weird subsetting or use-specific mods

# # NAs in data set
# sum(is.na(df)) # 16300
# df[1:100] = lapply(df[1:100], function(x){ifelse(is.na(x), mean(x, na.rm= T), x)})
# sum(is.na(df))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# helper functions
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


get_factor_columns = function(dataframe, type="index") {
  factorL = sapply(seq(dataframe), function(i) {
    is.factor(dataframe[[i]])
  })
  if (type == "logical") {
    return(factorL)
  } else if (type == "index") {
    return(seq(dataframe)[factorL])
  } else if (type == "name") {
    return(names(dataframe)[factorL])
  } else if (type == "data") {
    return(dataframe[factorL])
  } else {
    stop("invalid type argument to get_factor_columns")
  }
}

# returns list(x, y)
get_scaled_matrices = function(dataframe, resultscolname, center = TRUE, scale = TRUE) {
  x = as.matrix(dataframe[-match(resultscolname, names(dataframe))])
  x = scale(x, center, scale)
  
  # Y IS ASSUMED TO BE A FACTOR, FOR LOG ODDS PREDICTIONS
  y = as.factor(dataframe[[resultscolname]])
  return(list(x = x, y = y))
}

# returns: list(model, predictions, xscales list(center, scale))
get_log_caret_fits = function(dataframe, resultscolname, folds=5, repeats=1,
                              alphas = 1:10 * 0.1, lambdas = seq(0, 0.25, length.out=25)) {
  scaled_matrices = get_scaled_matrices(dataframe, resultscolname)

  # because I got a warning about factor levels being invalid variable names
  levels(scaled_matrices[["y"]]) = as.character(c(unique(scaled_matrices[["y"]])))


  param_grid = expand.grid(.alpha = alphas,
                           .lambda = lambdas)
  # classProbs = TRUE makes it be logodds # number = number of folds
  # repeats = number of epochs # summaryFunction = twoClassSummary for binary prediction
  # multiClassSummary for multiple class prediction
  
  control = trainControl(method="repeatedcv",
                         number=folds,
                         classProbs=TRUE,
                         repeats=repeats,
                         summaryFunction=twoClassSummary,
                         verboseIter=FALSE) # WILL NOT MAKE NOISE
  
  # metric = "ROC" for area under the curve.
  print("training caret_fit")
  caret_fit = train(x=scaled_matrices[["x"]],
                    y=scaled_matrices[["y"]],
                    method="glmnet",
                    metric="ROC",
                    tuneGrid=param_grid,
                    trControl=control)
  print("making predictions")
  
  predictions = predict(caret_fit$finalModel, newx = scaled_matrices[["x"]], s=caret_fit$finalModel$lambdaOpt)
  
  return(list(model = caret_fit$finalModel,
              predictions = predictions,
              xscales = list(center = attr(scaled_matrices[["x"]],"scaled:center"),
                          scale = attr(scaled_matrices[["x"]],"scaled:scale"))))
}

impute_distribution = function(dataframe, indexes) {
  for (i in indexes) {
    column = dataframe[[i]]
    NAs_L = is.na(column)
    distribution = sample(column[!NAs_L], sum(NAs_L), replace=TRUE)
    column[NAs_L] = distribution
    dataframe[[i]] = column
  }
  return(dataframe)
}

impute_average = function(dataframe, column_names) {
  for (name in column_names) {
    colvalues = dataframe[[name]]
    NAs_L = is.na(colvalues)
    dataframe[NAs_L, name] = mean(colvalues, na.rm = TRUE)
  }
  return(dataframe)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# data partitioning / cleanup
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
set.seed(1992)


# --------------------------------------------------------------------
# --------------------------------------------------------------------
# --------------------------------------------------------------------

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# visualizing (pre)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# visualize data

# # to get porportions sense:
# mosaicplot(table(vector, vector))

# # to spot correlations
# corrplot(cor(vector, vector), cl.pos="n")

# # to visualize coefficients
# corrplot(as.matrix(coef(model)), is.corr = FALSE, cl.pos="n")

# # to visualize model accuracy
# roc(actual, predicted, plot=TRUE)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# main run
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# visualizing (post) & conclusions
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# save your plot with the Plots menu

