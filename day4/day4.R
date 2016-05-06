install.packages("foreign")
library("foreign")
library("ggplot2")
library("dplyr")
getwd()
setwd("day4")

kidiq = foreign::read.dta("child.iq/kidiq.dta")
View(kidiq)

#3.1 - 3.3, skim 3.4

kid.scoret = kidiq$kid_score
kid.score1 = 78 + 12 * kidiq$mom_hs
kid.score = 26 + 0.6 * kidiq$mom_iq

kidiq %>% mutate(by_momhs = 78 + 12 * mom_hs,
by_momiq = 26 + 0.6 * mom_iq) %>%
ggplot(aes(col=kid_score)) + geom_jitter(aes(x=by_momiq, y=by_momhs)) + scale_colour_gradientn(5,colors=c("black","black","darkred","red","red"))

# # color blind palettes
# cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
# cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
# # To use for fills, add
# scale_fill_manual(values=cbPalette)
# # To use for line and point colors, add
# scale_colour_manual(values=cbPalette)

fit_3 <- lm(kid_score ~ mom_hs + mom_iq, data=kidiq)
plot(fit_3) # woah this gives some cool plots!
summary(fit_3)


iq.data = foreign::read.dta("child.iq/child.iq.dta")
str(iq.data)
iqModel = lm(ppvt~momage, iq.data)
plot(iqModel)
fitIqModel = cbind(iq.data, fitted(iqModel))
names(fitIqModel)[length(fitIqModel)] = "fitPPVT"
ggplot(fitIqModel, aes(x = momage, y = ppvt)) + geom_jitter(width=.5) + geom_line(aes(y = fitPPVT))

summary(iqModel)
table(iq.data$momage)
# this is basically useless, have kids when it makes sense for you to have a kid, default to delay a bit.
# at least if the question is between 18-27...


createIndicators = function(df) {
  for (colName in colnames(df)) {
    columnvalues = df[[colName]]
    if(is.factor(columnvalues)) {
      levelopts = levels(columnvalues[1])
      for(lvl in levelopts[2:length(levelopts)]) {
        #for(lvl in levelopts) {
        newcolumn = as.integer(columnvalues == lvl)
        df[paste(colName,"_",lvl, sep="")] = newcolumn
      }
    }
  }
  return( df)
}

#aaah should be indicators and stuff

iqModel2 = lm(ppvt ~ momage + educ_cat, data=iq.data)
plot(iqModel2)
fitIqModel = cbind(iq.data, fitted(iqModel2))
names(fitIqModel)[length(fitIqModel)] = "fitPPVT"
ggplot(fitIqModel, aes(x = momage, y = ppvt, color = educ_cat)) + geom_jitter(width=.5) + geom_line(aes(y = fitPPVT, group=educ_cat))
#Visual inspection makes it seem that education level is highly correlated with momage, and average score is significantly correlated with education level

iqModel2 = lm(ppvt ~ momage + educ_cat + momage*educ_cat, data=iq.data)
#plot(iqModel2)
fitIqModel = cbind(iq.data, fitted(iqModel2))
names(fitIqModel)[length(fitIqModel)] = "fitPPVT"
# filter(fitIqModel, momage > 20 & abs(ppvt - mean(ppvt)) < 20) %>%
ggplot(fitIqModel, aes(x = momage, y = ppvt, color = educ_cat)) + geom_jitter(width=.5) + geom_line(aes(y = fitPPVT, group=educ_cat)) + geom_boxplot(aes(group=momage))

summary(iqModel2)

iq.data2 = mutate(iq.data, highschoolPass = as.numeric(educ_cat > 1))

plot(x = iq.data2$highschoolPass, y = iq.data2$momage)
head(iq.data2)
mean(iq.data2$momage[iq.data2$highschoolPass==1])
mean(iq.data2$momage[iq.data2$highschoolPass==0])
sd(iq.data2$momage[iq.data2$highschoolPass==1])
sd(iq.data2$momage[iq.data2$highschoolPass==0])

iqModel2 = lm(ppvt ~ momage + highschoolPass + momage*highschoolPass, data=iq.data2)
#plot(iqModel2)
fitIqModel = cbind(iq.data2, fitted(iqModel2))
summary(iqModel2)
names(fitIqModel)[length(fitIqModel)] = "fitPPVT"
# filter(fitIqModel, momage > 20 & abs(ppvt - mean(ppvt)) < 20) %>%
ggplot(fitIqModel, aes(x = momage, y = ppvt, color = highschoolPass)) + geom_jitter(width=.5) + geom_line(aes(y = fitPPVT, group=highschoolPass))

iq.data = foreign::read.dta("child.iq/child.iq.dta")
idx = sample(1:400, 200)
train = iq.data[idx,]
test = iq.data[-idx,]
rownames(train) = 1:200
rownames(test) = 1:200

meanMomage = mean(train$momage)
meanPPVT = mean(train$ppvt)
make_sensible = function (df) {
  # save the offset and divisor
  mutate(df, high_schooled = as.numeric(educ_cat > 1),
         some_colleged = as.numeric(educ_cat > 2),
         graduated = as.numeric(educ_cat == 4)) %>%
    mutate(adj_age = momage - meanMomage, adj_ppvt = ppvt-meanPPVT) -> sensibilities
  return(sensibilities)
}
train = make_sensible(train)
test = make_sensible(test)
iqModelTrain = lm(adj_ppvt ~ adj_age + high_schooled + adj_age*high_schooled, data=train)

residuals = data.frame(testx = test["adj_age"] + meanMomage, resid = test["adj_ppvt"] - predict(iqModelTrain, test) + meanPPVT)
ggplot(residuals, aes(x=adj_age, y=adj_ppvt)) + geom_point() + geom_line(aes(y=meanPPVT))
fittedModel = data.frame(fits = fitted(iqModelTrain), educ_cat = train$educ_cat, adj_age = train$adj_age)
ggplot(test, aes(x = adj_age, y = adj_ppvt, color = educ_cat)) + geom_jitter(width=.5) + geom_line(data=fittedModel, aes(y = fits, group=educ_cat))


# 
# iqModelTrain = lm(adj_ppvt ~ adj_age + high_schooled + some_colleged + adj_age*some_colleged + adj_age*high_schooled + some_colleged*high_schooled, data=train)
# fittedModel = data.frame(fits = fitted(iqModelTrain), educ_cat = train$educ_cat, adj_age = train$adj_age)
# ggplot(test, aes(x = adj_age, y = adj_ppvt, color = educ_cat)) + geom_jitter(width=.5) + geom_line(data=fittedModel, aes(y = fits, group=educ_cat))
# summary(iqModelTrain)

# shrug

beautyData = read.csv("beauty/ProfEvaltnsBeautyPublic.csv")
View(beautyData)
names(beautyData)
beautyModel = lm(courseevaluation ~ btystdave + onecredit + btystdave*onecredit, data = beautyData)
fitBeauty = fitted(beautyModel)
beautyData = mutate(beautyData,fitBeauty)
# plot(beautyModel)
(ggplot(beautyData, aes(x = btystdave, y = courseevaluation, color = onecredit))
+ geom_jitter(width=.3)
+ geom_line(aes(y = fitBeauty, group = onecredit)))

mean(beautyData$courseevaluation[beautyData$onecredit == 1])
mean(beautyData$courseevaluation[beautyData$onecredit == 0])

#picking interesting variables
correlations = cbind(cor(beautyData,beautyData$courseevaluation), cor(beautyData,beautyData$btystdave))
colnames(correlations) = c("course", "beauty")
correlations
cbind(data.frame(correlations), thenames = rownames(correlations)) %>%
  filter(course > .15, beauty > .15) -> subresults
subresults

plot(x = beautyData$profevaluation, y = beautyData$courseevaluation)



beauty_df = beautyData
beauty_courseval_model = lm(courseevaluation  ~ btystdave + tenured + minority + age + female + nonenglish + lower + tenuretrack + formal, beauty_df)
min_beauty_courseval_model = lm(courseevaluation ~ 1, beauty_df)
formula_max_beauty_courseval_model = formula(beauty_courseval_model)
step(min_beauty_courseval_model, direction='forward', scope=formula_max_beauty_courseval_model)
best_courseval_model = lm(courseevaluation  ~ btystdave + female + nonenglish + tenuretrack + lower + formal + age, beauty_df)
summary(best_courseval_model)
summary(lm(courseevaluation ~ btystdave, beauty_df))
summary(min_beauty_courseval_model)

# beauty helps ~ .12 points per std of beauty or whatever this is
# formality maybe helps about the same, not statistically significant
# tenuretrack lowers average ~0.2 points
# noneglish speaking -0.3 points
# female -0.2 points
# only 10% of variation explained.... and there's a lot of variation

ggplot(beauty_df) + geom_density(aes(x=courseevaluation))
# possibly teachers are actually rated based on how good they are instead of random other things

library("GGally")
beauty_df %>% select(tenured, minority, age, btystdave, courseevaluation, female)

plot(beauty_df$courseevaluation, log(beauty_df$didevaluation))
# indicators didevaluation, courseevaluation
summary(lm(courseevaluation ~ log(beauty_df$didevaluation) + lower, beauty_df))
# lower = 0.14483 slope, p=0.0827
# log(didevaluation) = -0.04661 slope, p = 0.0827
ggplot(beauty_df, aes(x=courseevaluation, y = log(didevaluation))) + geom_point() + geom_smooth(method=lm)

m = matrix(1:100, nrow=5, ncol = 20)
as.numeric(m)
dim(m) = c(20, 5)
m
as.numeric(m)

# dot-multiply for the WIIIIINN
m = matrix(1:100, nrow=10)
m * diag(10)


min_matrix = function(n, m) {
  mod_matrix = matrix(rep(0,n*m), nrow=n)
  for (i in 1:n) {
    for (j in 1:m) {
      mod_matrix[i,j] = min(i,j)
    }
  }
  return(mod_matrix)
}

min_matrix(5,3)
m = 5
n = 3
mr = matrix(rep(1:m, n), nrow = n, byrow=TRUE)
mc = matrix(rep(1:n, m), nrow = n)

min_matrix = pmin(mr,mc)
mr
mc
min_matrix

symmetrical = function(m) {
  if(nrow(m)!=ncol(m))
    return(FALSE)
  check = m != t(m)
  return(sum(check) == 0)
}

symmetrical(diag(5))
symmetrical(diag(4) + matrix(c(1:16), nrow=4))
symmetrical(matrix(1:20, nrow=5))

trace = function(mat) {
  dMat = diag(x = 1, nrow = nrow(mat),ncol = ncol(mat))
  return(sum(dMat*mat))
}
m = matrix(1:10, nrow = 2)
m
m2 = matrix(13:22, nrow = 2)
m2
resultAdd = trace(m+m2)-(trace(m)+trace(m2))
resultAdd
resultMul = trace(m*m2)-(trace(m)*trace(m2))
resultMul

# 1 * 13 + 4 * 16
#    !=
# (1 + 4)(13 + 16)

mystery = function(x) {
  matrix(c(cos(x), -sin(x), sin(x), cos(x)), nrow=2)
}

# df x, cosx, -sinx, sinx, cosx
xvals = seq(-5,5,0.1)
df = data.frame(x = xvals, cosx = cos(xvals), negsinx = -sin(xvals), sinx = sin(xvals))
(ggplot(df, aes(x=xvals)) + geom_point(aes(y=cosx, color="blue"))
  + geom_point(aes(y=negsinx, color="red"))
  + geom_point(aes(y=sinx, color="green")))
# periodic? It's a periodic function or three

example = list(1,2,3,4)

matrify = function(mlist) {
  for (i in 1:length(mlist)) {
    mlist[i] = list(mlist[i])
  }
  matrix(mlist, nrow=2)
}

matrify(example)
install.packages("timeit")
library("timeit")
matrix_mult = function(multA, multB)
{
  if(nrow(multA) != nrow(multB) | ncol(multA) != ncol(multB)){
    print("Oh no...")
  print("nooo....")
  print("NOOOOOOOO!!!!!!")
  }
  multMat = matrix(nrow = nrow(multA), ncol = ncol(multA))
  for(i in 1:nrow(multMat)){
    for(j in 1:ncol(multMat)){
      multMat[i,j] = multA[i,j]*multB[i,j]
    }
  }
  return(multMat)
}

m = matrix_mult(matrix(1:9, nrow = 3),matrix(1:9, nrow = 3))
m
t = timeit(matrix_mult)
mean(t)
