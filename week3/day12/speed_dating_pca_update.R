library(ggplot2)
library(dplyr)
library(pROC)
library(corrplot)

spd_df = read.csv("C:/Users/User/Documents/GitHub/Signal-Data-Science/speeddating-aggregated.csv")

##Take out NA rows
spd_df = select(spd_df, sports:yoga, gender,race,career_c)
spd_df = spd_df[rowSums(is.na(spd_df))==0,]

features = select(spd_df, sports:yoga)
targets = select(spd_df,gender, race, career_c)

## run PCA
p = prcomp(features, scale=TRUE)

qplot(1:17, p$sdev)
corrplot(p$rotation[,1:10])
# p1 is artsy person
# p2 is bros
# p3 is inactive
# p4 is fancypants
# p5 is outgoing
# p6 ?????


corrplot(cor(features, p$x[,1:4]))


## integer, dataframe, vector -> 
##Folding the data
set.seed(13)
fold_n = function(n, features, targets){
  fold_ind = sample(nrow(features)) %% n + 1 
  pca = prcomp(features, scale=TRUE)
  train_target = vector(mode="list",n)
  test_target = vector(mode="list",n)
  train_pca = vector(mode="list",n)
  test_pca = vector(mode="list",n)
  for (i in 1:n){
    train_target[[i]]  = targets[fold_ind != i]
    test_target[[i]]  = targets[fold_ind == i]
    train_pca[[i]]  = data.frame(pca$x[fold_ind != i,])
    test_pca[[i]] = data.frame(pca$x[fold_ind == i,])
  }
  return(
    list(
      train_target, test_target,
      train_pca, test_pca
    )
  )
}


## 
differences_gender = vector(mode="list", 10)
differences_career = vector(mode="list", 10)
differences_race = vector(mode="list", 10)

get_pca_rmse = function(features, target, n_fold, pca_n){
  folded = fold_n(n_fold,features,target)
  train_target = folded[[1]]
  test_target = folded[[2]]
  train_pca = folded[[3]]
  test_pca = folded[[4]]
  fit_objects = vector(mode="list", pca_n)
  coefficient_pvals = vector(mode="list", pca_n)
  for (n in 1:pca_n){
    fit_objects[[n]] = vector(mode="numeric", n_fold)
    for (i in 1:n_fold){
      fit_df = as.data.frame(cbind(train_target[[i]],train_pca[[i]][,1:n]))
      colnames(fit_df)[1]=c("target")
      fit = glm(target~., fit_df, family=binomial)
      
      test_df = as.data.frame(cbind(test_target[[i]],test_pca[[i]][,1:n]))
      colnames(test_df)[1]=c("target")
      # alternative 
      predictions = predict(fit, test_df)
      
      fit_objects[[n]][i] = auc(roc(test_target[[i]], predictions))
    }
    
    pca = prcomp(features, scale=TRUE)
    fit_df = as.data.frame(cbind(target, pca$x[,1:n]))
    colnames(fit_df)[1] = "target"
    fit = glm(target ~ ., fit_df, family=binomial)
    coefficient_pvals[[n]] = summary(fit)$coefficients[,1]
  }
  return(list(auc = fit_objects,
              coefficient_pvals = coefficient_pvals))
}


# this is for gender

silenced = get_pca_rmse(features, targets$gender, 10, 17)
auc_df = as.data.frame(silenced$auc)
colnames(auc_df) = as.character(1:17)
means = colMeans(auc_df)
sds = sapply(auc_df, sd)

ggplot(data.frame(), aes(1:17, means)) + geom_point() + geom_errorbar(aes(ymin= means - sds, ymax = means + sds))


gender_coefficients = as.data.frame(sapply(silenced$coefficient_pvals, function(itemvector) {
                                                            val = rep(0, 18)
                                                            val[1:length(itemvector)] = itemvector
                                                            return(val)
                                                          }))

corrplot(as.matrix(gender_coefficients), is.corr=FALSE)


# gender_df = rbind(cbind(gender_coefficients$V1, type="V1"),
#                   
#                   
#                   )


## this is for race
race_df = filter(spd_df, race==2 | race==4)
features_race = select(race_df, sports:yoga)
target_race = race_df$race
target_race = as.numeric(target_race==4)

race_analysis = get_pca_rmse(features_race, target_race, 10, 17)
race_auc_df = as.data.frame(race_analysis$auc)
colnames(race_auc_df) = as.character(1:17)
means_race = colMeans(race_auc_df)
race_sds = sapply(race_auc_df, sd)

ggplot(data.frame(), aes(1:17, means_race)) + geom_point() + geom_errorbar(aes(ymin= means_race - race_sds, ymax = means_race + race_sds))

race_coefficients = as.data.frame(sapply(race_analysis$coefficient_pvals, function(itemvector) {
  val = rep(0, 18)
  val[1:length(itemvector)] = itemvector
  return(val)
}))

corrplot(as.matrix(race_coefficients), is.corr=FALSE)


## this is for career
career_df = filter(spd_df, career_c==2 | career_c==4)
features_career = select(career_df, sports:yoga)
target_career = career_df$career_c
target_career = as.numeric(target_career==4)

career_analysis = get_pca_rmse(features_career, target_career, 10, 17)
career_auc_df = as.data.frame(career_analysis$auc)
colnames(career_auc_df) = as.character(1:17)
means_career = colMeans(career_auc_df)
career_sds = sapply(career_auc_df, sd)

ggplot(data.frame(), aes(1:17, means_career)) + geom_point() + geom_errorbar(aes(ymin= means_career - career_sds, ymax = means_career + career_sds))


career_coefficients = as.data.frame(sapply(career_analysis$coefficient_pvals, function(itemvector) {
  val = rep(0, 18)
  val[1:length(itemvector)] = itemvector
  return(val)
}))

corrplot(as.matrix(career_coefficients), is.corr=FALSE)
