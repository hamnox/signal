library(tidyr)
library(ggplot2)
library(dplyr)
library(Rmisc)
library(caret)
library(corrplot)
library(rpart)
# library(rattle)
library(randomForest)
library(earth)
library(Cubist)
library(caretEnsemble)

kaggle_bike_train = read.csv("datasets/kaggle_bike_train.csv")

sum(is.na(kaggle_bike_train))
sapply(kaggle_bike_train, class)

head(kaggle_bike_train$datetime)

df = kaggle_bike_train
mutate_data = function(df) {
  df$hour = as.numeric(substr(as.character(kaggle_bike_train$datetime), 12, 13))
  df$datetime = strptime(kaggle_bike_train$datetime, format="%Y-%m-%d %H:%M:%S")
  # df$dayn = substr(as.character(kaggle_bike_train$datetime), 1, 4)
  # df$day = factor(strftime(df$datetime, "%A"), c("Sunday", "Monday",
                                                  # "Tuesday", "Wednesday",
                                                  # "Thursday",
                                                  # "Friday", "Saturday"))
  df$datetime = NULL
  df$weather = NULL
  df$clear = as.numeric(kaggle_bike_train$weather == 1)
  df$mist = as.numeric(kaggle_bike_train$weather == 2)
  df$light = as.numeric(kaggle_bike_train$weather == 3)
  df$heavy = as.numeric(kaggle_bike_train$weather == 4)
  
  df$season = NULL
  df$spring = as.numeric(kaggle_bike_train$season == 1)
  df$summer = as.numeric(kaggle_bike_train$season == 2)
  df$fall = as.numeric(kaggle_bike_train$season == 3)
  df$winter = as.numeric(kaggle_bike_train$season == 4)
  
  return (df)
  
  ### AAGGGH I WANT TO DO SOME LAG VARS
  ## count of day before
  ## count of some similar window of time before
  # I mean, SEE HERE:
  # corrplot(cor(df, df[c(25:nrow(df), 1:24),]), cl.pos="n")
  
  # also I want to rewrite the counts so they're per hour instead
  # of cumulative like casual and registered, have the cumulative
  # calculated separately
}
df = mutate_data(df)

corrplot(cor(df), cl.pos="n")

ensemble_methods = c("glmnet", "kknn", "rpart", "gbm", "earth")
ensemble_control = trainControl(method="repeatedcv", repeats=2,
                                number=3, verboseIter = TRUE,
                                savePredictions = "final")
ensemble_tunes = list(
  glmnet=caretModelSpec(method="glmnet", tuneLength=10),
  kknn=caretModelSpec(method="kknn", tuneLength=10),
  rpart=caretModelSpec(method="rpart", tuneLength=10),
  gbm=caretModelSpec(method="gbm", tuneGrid=expand.grid(n.trees=500,
                                                        shrinkage=10^seq(-3, 0, 1),
                                                        interaction.depth=seq(3),
                                                        n.minobsinnode=seq(10, 50, 10))),
  earth=caretModelSpec(method="earth", tuneGrid=expand.grid(degree=1:5,
                                                   nprune=10:20))
  
)


ensemble_fits = caretList(count ~ ., df,
                          trControl=ensemble_control,
                          methodList=ensemble_methods,
                          tuneList=ensemble_tunes)
