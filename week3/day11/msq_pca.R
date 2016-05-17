library('dplyr')
library('ggplot2')
library(corrplot)
#install.packages("DAAG")
library(DAAG)

##Load msq dataset
df_msq = msq

##Take out NA columns with too many NAs
na_lim = 100
df_msq = df_msq[,  colSums(is.na(df_msq))<na_lim]

##Take out NA rows
df_msq = df_msq[rowSums(is.na(df_msq))==0,]

##Make features dataframe
features= select(df_msq, active:wide.awake)
target = select(df_msq, Extraversion, Neuroticism)

## run PCA
p = prcomp(scale(features))

## top 10 loading
top = function(p, n){
  loading_matrix = p$rotation
  loading_vector = loading_matrix[,n]
  names(loading_vector) = rownames(loading_matrix)
  
  grabby = order(abs(loading_vector), decreasing=TRUE)[1:10]
  return(loading_vector[grabby])
}

# this is when it would come in handy to be working in markdownR
qplot(1:66, p$sdev)

# looking at the numbers
plots = lapply(1:7, function (x) {
  correlations = top(p, x)
  corrplot(as.matrix(correlations), is.corr=FALSE)
})

## P1 = Zest
## P2 = unStressed
## P3 = unSerenity
## P4 = Attentive
## P5 = dominant
## P6 = emotive?
## P7 = anticipatory?

# interpretability is low overall

##root mean square error function
rmse=function(x,y){
  return(sqrt(mean(((x-y)^2))))
}

##Generate fold assignment (6 instead of 4 made)
N_fold = 10
train_features = vector(mode="list",N_fold)
test_features = vector(mode="list",N_fold)
train_Extraversion = vector(mode="list",N_fold)
test_Extraversion = vector(mode="list",N_fold)
train_Neuroticism = vector(mode="list",N_fold)
test_Neuroticism = vector(mode="list",N_fold)
train_pca = vector(mode="list", N_fold)
test_pca = vector(mode="list", N_fold)

##Folding \the data
set.seed(1)
fold_ind = sample(nrow(features)) %% N_fold + 1   ##placed here in edit; should not be rerun every time
for (i in 1:N_fold){
  # fold_ind = sample(nrow(features_df)) %% N_fold + 1
  train_features[[i]] = features[fold_ind != i,]
  test_features[[i]] = features[fold_ind == i,]
  train_Extraversion[[i]]  = target$Extraversion[fold_ind != i]
  test_Extraversion[[i]]  = target$Extraversion[fold_ind == i]
  train_Neuroticism[[i]]  = target$Neuroticism[fold_ind != i]
  test_Neuroticism[[i]]  = target$Neuroticism[fold_ind == i]
  train_pca[[i]]  = data.frame(p$x[fold_ind != i,1:15])
  test_pca[[i]] = data.frame(p$x[fold_ind == i,1:15])
}


differences_Ex = vector(mode="list", N_fold)
differences_Ne = vector(mode="list", N_fold)

# Using "p" from before, as PCA to be run on the whole thing
get_pca_rmse = function(pca_n) {
  for (i in 1:N_fold){
    print(paste("fold number", i))
    fit_df = as.data.frame(cbind(train_Neuroticism[[i]], train_Extraversion[[i]], train_pca[[i]][,1:pca_n]))

    colnames(fit_df)[1:2] = c("Neuroticism", "Extraversion")
    
    fit1 = lm(Neuroticism ~ . -Extraversion, fit_df)
    fit2 = lm(Extraversion ~ . -Neuroticism, fit_df)
    
    test_df = as.data.frame(cbind(test_Neuroticism[[i]], test_Extraversion[[i]], test_pca[[i]][,1:pca_n]))
    names(test_df)[1:2] = c("Neuroticism", "Extraversion")
    predicted1 = predict(fit1, test_df)
    predicted2 = predict(fit2, test_df)
    differences_Ex[[i]]= predicted1 - test_df$Extraversion      ## later add another dimension
    differences_Ne[[i]]= predicted1 - test_df$Neuroticism
  }
  return(c(
    error_Ex = sqrt(mean((unlist(differences_Ex))^2)),
    error_Ne = sqrt(mean((unlist(differences_Ne))^2))
  ))
}

pca_rmses = matrix(0, nrow=15, ncol=2)
colnames(pca_rmses) = c("Extraversion_error", "Neuroticism_error")

for (i in 1:15) {
  pca_rmses[i,] = get_pca_rmse(i)
}

ggplot(as.data.frame(pca_rmses), aes(x=1:15)) + geom_point(aes(y=Neuroticism_error), color="blue") + geom_point(aes(y=Extraversion_error), color="red")
# 
# > min(concise_answer$rmse_Ex)
# [1] 3.910266
# > min(concise_answer$rmse_Ne)
# [1] 5.832635
# Doing better on Neuroticism (with increasing PCA), doing worse than extraversion (which does increasingly worse with more PCA)
