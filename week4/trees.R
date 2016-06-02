library("readr")
library("ggplot2")
library("glmnet")
library("caret")
library("dplyr")
library("kknn")
library("rpart")
library("randomForest")
library("gbm")
red_df = read.csv("C:/Users/User/Documents/GitHub/Signal-Data-Science/winequality-red.csv", sep=";")
white_df = read.csv("C:/Users/User/Documents/GitHub/Signal-Data-Science/winequality-white.csv", sep=";")



## Replace spaces with underscores
replace_str = function(str_li){
  new_li = vector(mode="list", n=length(str_li))
  for (i in 1:length(str_li)){
    new_li[[i]] = paste(unlist(strsplit(str_li, " ")), collapse="_")
  }
}

## Plots wine chemical stat vs quality

ggplot(data=white_df, aes(x=white_df[[1]], y=white_df[[12]]))+geom_point()+geom_smooth()+xlab(colnames(white_df)[1])+ylab (colnames(white_df)[12])

plot_one  = function(df, x, y=12){
  ggplot(data=df, aes(x=df[[x]], y=df[[y]]))+geom_point()+geom_smooth()+xlab(colnames(df)[x])+ylab (colnames(df)[y])
  
}

for (i in 1:11){
  print(plot_one(white_df, i))
}

# Fixed acidity not linear
# Volatile acidity may be decreasing and linear
# Citric acid has a bump in the middle
# Residual sugar has lots of bumps
# Chlorides are all over
# etc. mostly non-linear

control = trainControl(method="repeatedcv", repeats=1, number=3,
                       verboseIter=TRUE)
caret_fit = train(select(white_df, -quality), white_df$quality, trControl=control, method="glmnet", tuneLength=10)

best_lin_coef = round(coef(caret_fit$finalModel, s=caret_fit$bestTune$lambda),2)
lin_rmse = min(caret_fit$results$RMSE)
# This is extremely broken. Nothing except density matters, rmse at 0.75

## K nearest neighbours

control = trainControl(method="repeatedcv", repeats=1, number=3,
                       verboseIter=TRUE)
caret_fit_knn = caret_fit = train(select(white_df, -quality), white_df$quality, trControl=control, method="kknn", tuneLength=10)
knn_pred = predict(caret_fit_knn$finalModel, select(white_df, -quality))
# knn_attempt = kknn(white_df$quality~select(white_df, -quality), k=13, distance=2)
knn_calc_rmse = RMSE(white_df$quality, knn_pred)

# Slightly different rmse, at 0.70 (cross-validated)
# rmse at 0.44 when calculated

# Regression Tree
r_tree_white = rpart(quality~., data=white_df)
r_tree_red = rpart(quality~., data=red_df)
print(r_tree_white)
print(r_tree_red)

control = trainControl(method="repeatedcv", repeats=1, number=3,
                       verboseIter=TRUE)
caret_fit_rpart = caret_fit = train(select(white_df, -quality), white_df$quality, trControl=control, method="rpart", tuneLength=10)
rpart_rmse = min(caret_fit_rpart$results$RMSE)
rpart_calc_rmse
rpart_pred = predict(caret_fit_rpart$finalModel, select(white_df, -quality))
rpart_calc_rmse = RMSE(white_df$quality, rpart_pred)
# rmse at 0.75 retreived
# rmse at 0.73 calculated

# Random Forest
p = ncol(white_df)-1
r_forest_p = randomForest(quality~., data=white_df, mtry=p)
r_forest_rp = randomForest(quality~., data=white_df, mtry=floor(sqrt(p)))
p_forest_pred = predict(r_forest_p)
rp_forest_pred = predict(r_forest_rp)
p_forest_rmse = RMSE(p_forest_pred, white_df$quality)
# 0.590
rp_forest_rmse = RMSE(rp_forest_pred, white_df$quality)
# 0.585

# Gradient Boosted Trees
param_grid = expand.grid(n.trees=500, shrinkage=10^seq(-3,0,1), interaction.depth=1:3, n.minobsinnode=seq(10,50,10))
control = trainControl(method="repeatedcv", repeats=1, number=3,
                       verboseIter=TRUE)
caret_fit_gbm = train(quality~., white_df, trControl=control, method="gbm", tuneGrid=param_grid)
gbm_fit = gbm(quality~., data=white_df, n.trees=5000, interaction.depth=3, shrinkage=0.1, n.minobsinnode=10, cv.folds=3)
min(gbm_fit$cv.error)
# 0.45
qplot(1:5000, (gbm_fit$cv.error))
gbm_pred = predict(gbm_fit, select(white_df, -quality))
gbm_rmse = RMSE(gbm_pred, white_df$quality)
# 0.409