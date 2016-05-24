library(tidyr)
library(ggplot2)
library(dplyr)
library(Rmisc)
library(caret)
library(corrplot)
library(rpart)
# library(rattle)
library(randomForest)

# load data
red_df = read.csv("datasets/winequality-red.csv", sep=";")
white_df = read.csv("datasets/winequality-white.csv", sep=";")

# remove NAs that don't exist
sum(is.na(red_df))
sum(is.na(white_df))
str(red_df)
str(white_df)
silenced = read.delim("datasets/winequality-white.csv", sep=";")
sum(is.na(silenced))

# plot stuff


# this didn't work
# tall_red = tidyr::gather(red_df, property_type, value, fixed.acidity:alcohol)
# base = ggplot(tall_red, aes(color=property_type)) + geom_jitter(aes(x=value, y=quality), alpha=0.3)
# base

# or this
for (p in names(select(white_df, -quality))) {
  print(ggplot(white_df, aes(x=white_df[[p]], y=quality)) 
        + xlab(p) + geom_point() + geom_smooth())
}
# fixedacity has an outlier
# volatileacidity is bumpy
# citric acid very nonlinear, residual sugar too and has outliers
# chlorides nonlinear
# wow just lots of nonlinear stuff

control = trainControl(method="repeatedcv", repeats=1, number=3,
                       verboseIter=TRUE)
caret_fit = train(select(white_df, -quality), white_df$quality,
                  trControl=control, method="glmnet", tuneLength=10)

lin_coefs = coef(caret_fit$finalModel, s=caret_fit$bestTune$lambda)
#corrplot(as.matrix(lin_coefs[-1,]), is.corr=FALSE)
# DENSITY IS HIGHLY ANTI-CORRELATED

# linear model RMSE
caret_preds = predict(caret_fit$finalModel, newx = as.matrix(select(white_df, -quality)), s=caret_fit$bestTune$lambda)
caret_RMSE = RMSE(caret_preds, white_df$quality)

# residual plot
qplot(white_df$quality, caret_preds - white_df$quality)

### KNN
# install.packages("kknn")
library("kknn")

# fitting a KNN model
kknn_caret_fit = train(select(white_df, -quality), white_df$quality,
                       trControl=control, method="kknn", tuneLength=10)

# Now to get the RMSE of that
kknn_preds = predict(kknn_caret_fit$finalModel, select(white_df, -quality))
RMSE(kknn_preds, white_df$quality) # 0.4411503, better!
# kknn_caret_fit$results$RMSE, cross validated with different k values
qplot(white_df$quality, kknn_preds - white_df$quality)




#### FITTING REDS KNN MODEL
r_kknn_caret_fit = train(select(red_df, -quality), red_df$quality,
                       trControl=control, method="kknn", tuneLength=10)

# Now to get the RMSE of that
r_kknn_preds = predict(r_kknn_caret_fit$finalModel, select(red_df, -quality))
RMSE(r_kknn_preds, red_df$quality) # 0.5077229
# kknn_caret_fit$results$RMSE, cross validated with different k values

qplot(red_df$quality, r_kknn_preds - red_df$quality)

# random forests
# mtry matters, usually use mtry = floor(sqrt(p)) or mtry=floor(p/3)
# use former when p/3 rounds to 1-2 or p is << n. always wise to try mtry=p

p = ncol(white_df)-1
wpforest = randomForest(quality ~ ., white_df, mtry=p)
wsqrtforest = randomForest(quality ~ ., white_df, mtry=floor(sqrt(p)))

# no dataframe put in does out of bag error
wp_preds = predict(wpforest)
wsqrt_preds = predict(wsqrtforest)

wp_RMSE = caret::RMSE(wp_preds, white_df$quality)
wsqrt_RMSE = caret::RMSE(wsqrt_preds, white_df$quality)
wp_RMSE # 0.5913973
wsqrt_RMSE # 0.5854594
# not bad, not as good as KNN


# gradient-boosted trees
library(gbm)

# getModelInfo("gbm")
control = trainControl(method="repeatedcv", repeats=1, number=3,
                       verboseIter=TRUE)
gbm_hyperparameters = train(select(white_df, -quality), white_df$quality,
                  trControl=control, method="gbm",
                  tuneGrid = expand.grid(n.trees=500,
                                         shrinkage=10^seq(-3, 0, 1),
                                         interaction.depth=seq(3),
                                         n.minobsinnode=seq(10, 50, 10)))
# The final values used for the model were n.trees = 500,
# interaction.depth = 3, shrinkage = 0.1 and n.minobsinnode = 10. 

gbm_fit = gbm(quality ~ ., data=white_df, n.trees=5000, interaction.depth = 3,
    shrinkage = 0.1, n.minobsinnode = 10, cv.folds = 3)

# min cv RMSE, then plot all cv-ed rmses
min(gbm_fit$cv.error) # 0.4721768, better than KNN even!
qplot(y=gbm_fit$cv.error, x=seq(5000))

# how much does it overfit? time to find out
gbm_preds = predict(gbm_fit, select(white_df, -quality))
RMSE(gbm_preds, white_df$quality) # 0.5312718, so you can see some overfit
