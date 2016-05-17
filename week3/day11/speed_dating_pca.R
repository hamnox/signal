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
    train_target[[i]]  = target[fold_ind != i]
    test_target[[i]]  = target[fold_ind == i]
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


# differences_Ex = vector(mode="list", 10)
# differences_Ne = vector(mode="list", 10)
# 
# 
# # Using "p" from before, as PCA to be run on the whole thing
# get_pca_rmse = function(pca_n) {
#   for (i in 1:N_fold){
#     print(paste("fold number", i))
#     fit_df = as.data.frame(cbind(train_Neuroticism[[i]], train_Extraversion[[i]], train_pca[[i]][,1:pca_n]))
#     
#     colnames(fit_df)[1:2] = c("Neuroticism", "Extraversion")
#     
#     fit1 = lm(Neuroticism ~ . -Extraversion, fit_df)
#     fit2 = lm(Extraversion ~ . -Neuroticism, fit_df)
#     
#     test_df = as.data.frame(cbind(test_Neuroticism[[i]], test_Extraversion[[i]], test_pca[[i]][,1:pca_n]))
#     names(test_df)[1:2] = c("Neuroticism", "Extraversion")
#     predicted1 = predict(fit1, test_df)
#     predicted2 = predict(fit2, test_df)
#     differences_Ex[[i]]= predicted1 - test_df$Extraversion      ## later add another dimension
#     differences_Ne[[i]]= predicted1 - test_df$Neuroticism
#   }
#   return(c(
#     error_Ex = sqrt(mean((unlist(differences_Ex))^2)),
#     error_Ne = sqrt(mean((unlist(differences_Ne))^2))
#   ))
# }
