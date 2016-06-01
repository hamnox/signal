library(dplyr)
library(ggplot2)

df = read.csv("2016_lw_survey_public_release_3.csv", na.strings=c("", "N/A"))

# Restrict to consent research
df = df %>% filter(ResearchConsent == "Yes", !is.na(Age), Depression != "", !is.na(Depression)) 
dim(df)

# Drop columns with only a single unique value
single_uniques = sapply(df, function(col) { length(unique(col)) == 1 })
df = df[,!single_uniques]

# Drop questions with "Calibration", "CharityDonations", and "Peak" in their titles
df = select(df, -starts_with("Calibration"), -starts_with("CharityDonations"), -starts_with("Peak"))

# Drop the columns corresponding to answer choices that users wrote in.
df = select(df, -matches(".*other.*|.*comment.*|.*WriteIn.*|IQType"))
prop_uniques = sapply(df, function(col) { length(unique(col))/sum(!is.na(col)) })
df = df[prop_uniques < .8]

# Remove write-in choices under "What are the biggest problems with LW?"
# "What would you want from a successor?"

df = select(df, -one_of("PhilosophyIssuesNow.7.", "PhilosophyIssuesNow.8."))
df = select(df, -one_of("SuccessorCommunity.6.", "SuccessorCommunity.7.", 
                        "SuccessorCommunity.8."))
df = select(df, -one_of("SuccessorPhilosophy.6.", "SuccessorPhilosophy.7.", 
                        "SuccessorPhilosophy.8."))

# Convert each factor into a factor omitting "" and "N/A"
for (i in 1:ncol(df)) {
  if (is.factor(df[, i])) {
    df[[i]] = addNA(df[[i]], ifany = TRUE)
  }
}

numeric = sapply(df, is.numeric)
numeric_cols = df[,numeric]
summary(numeric_cols)  # Age, IQ, IQAge, SAT, ProbQuestions.1, 
  # ProbQuestions.2, Singularity Year, UnemploymentYear

# Replace outlier values with NAs

dena = function(v) {
  return(v[!is.na(v)])
}

df$Age[df$Age > 150] = NA
df$IQ[df$IQ > 250] = NA
df$IQ[df$IQ < 50] = NA
badages = df$IQAge < 4 | df$IQAge > 150
badages[is.na(badages)] = FALSE
df[badages, c("IQ","IQAge")] = c(NA, NA)
df$SAT[df$SAT > 1600] = NA

prob_question_names = paste0("ProbabilityQuestions.", 1:12, ".")

sum(df$`ProbabilityQuestions.1.` == 0.5, na.rm = TRUE)
bad_probs = (df$`ProbabilityQuestions.1.` == 0.5 
             | df$`ProbabilityQuestions.1.` == 60)  # Mel wants to keep the 51
bad_probs[is.na(bad_probs)] = FALSE
df[bad_probs, prob_question_names] = NA 

df$`ProbabilityQuestions.2.`[df$`ProbabilityQuestions.2.` < 0] = NA
df$SingularityYear[df$SingularityYear > 1000000] = NA
df$SingularityYear[df$SingularityYear < 2016] = NA

df$UnemploymentYear[df$UnemploymentYear < 2016 | df$UnemploymentYear > 20000] = NA

# replace money things with log(variable + 1)
df = df %>% mutate(Income = log(Income + 1), IncomeCharityPortion = log(IncomeCharityPortion + 1), XriskCharity = log(XriskCharity + 1))

backup = df
# convert yes/no questions to 1/0 features
for (i in 1:ncol(df)) {
  col = df[[i]]
  if (is.factor(col)) {
    # If three levels and one of them is NA
    if (length(levels(col)) == 3 & anyNA(levels(col))) {
      df[[i]] = as.numeric(col) - 1
      df[df[[i]] == 2, i] = NA
    } else { 
      if (length(levels(col)) == 2) {
        df[[i]] = as.numeric(col) - 1
        df[df[[i]] == 2, i] = NA
      }
    } 
  }
}

# Make numeric features from factors with meaningful levels indicating
# strength of affirmative answer: No = 0 / Not formally = 1 / Yes = 2 / NA
mental_names = names(select(df, OCD:SubstanceUseDisorder))
for (name in mental_names) {
  df[[name]] = as.numeric(df[[name]]) - 1
  df[[name]][df[[name]] == 3] = NA
}

summary(select(df, OCD:SubstanceUseDisorder))

# No, Yes, Yes but only, NA
# 0, 1, 2, 3 <- as.numeric() - 1
# 0, 2, 1, NA <- final

# Less, More, Same, NA
# 1, 2, 3, 4 <- original
# 0, 2, 1, NA <- final
mental_names = names(select(df, SubstanceUseWonder:SubstanceUseWorry, 
                            starts_with("SuccessorCommunity")))
for (name in mental_names) {
   if (is.factor(df[[name]])) {
     df[[name]] = as.numeric(df[[name]]) - 1
     
     # Yes: want this to map eventually to 2.
     df[[name]][df[[name]] == 1] = 5  # "Yes" --> Temp value
     df[[name]][df[[name]] == 2] = 1  # "Yes but only" --> 1
     df[[name]][df[[name]] == 5] = 2  # Temp value --> 2
     
     df[[name]][df[[name]] == 3] = NA
   }
}

# check changes
summary(select(df, SubstanceUseWonder:SubstanceUseWorry))

# Remove random url in results
df$BlogsRead2.4.[df$BlogsRead2.4. == "http://kajsotala.fi/"] = NA
df$BlogsRead2.4. = factor(df$BlogsRead2.4.)

# Almost Never, Never, Never Heard Of It, Rarely, Regular, Sometimes, NA
# 0, 1, 2, 3, 4, 5, 6 <- as.numeric() - 1
# 2, 1, 0, 3, 5, 4, NA
blogs_names = names(select(df, starts_with("BlogsRead")))
for (name in blogs_names) {
  temp = rep(NA, length(df[[name]]))
  temp[df[[name]] == "Almost Never"] = 2
  temp[df[[name]] == "Never"] = 1
  temp[df[[name]] == "Never Heard Of It"] = 0
  temp[df[[name]] == "Rarely"] = 3
  temp[df[[name]] == "Sometimes"] = 4
  temp[df[[name]] == "Regular Reader"] = 5
  df[[name]] = temp
  #print(levels(df[[name]]))
}

summary(select(df, starts_with("BlogsRead")))

story_names = names(select(df, starts_with("StoriesRead")))
for (name in story_names) {
  temp = rep(NA, length(df[[name]]))
  temp[df[[name]] == "Partially And Abandoned"] = 2
  temp[df[[name]] == "Never"] = 1
  temp[df[[name]] == "Never Heard Of It"] = 0
  temp[df[[name]] == "Partially And Intend To Finish"] = 3
  temp[df[[name]] == "Whole Thing"] = 4
  df[[name]] = temp
  #print(levels(df[[name]]))
}  

summary(select(df, starts_with("StoriesRead")))

tmp = select(df, AbortionLaws.SQ001.:MinimumWage.SQ001., GreatStagnation.SQ001.)

# Example
# [1,] "Lean Pro-Choice"  -> 3           
# [2,] "Lean Pro-Life"    -> 1              
# [3,] "No strong opinion"-> 2     
# [4,] "Pro-Choice"       -> 4     
# [5,] "Pro-Life"         -> 0
issue_names = names(select(df, AbortionLaws.SQ001.:MinimumWage.SQ001., GreatStagnation.SQ001.))
for (name in issue_names) {
  temp = rep(NA, length(df[[name]]))
  df[[name]] = as.numeric(df[[name]])
  temp[df[[name]] == 1] = 3
  temp[df[[name]] == 2] = 1
  temp[df[[name]] == 3] = 2
  temp[df[[name]] == 4] = 4
  temp[df[[name]] == 5] = 5
  df[[name]] = temp
}

summary(select(df, AbortionLaws.SQ001.:MinimumWage.SQ001., GreatStagnation.SQ001.))

tmp = select(df, Feminism.SQ001.:HumanBiodiversity.SQ001.)

# Example
# [1,] "Favorable"         "Favorable"          "Favorable"         3     
# [2,] "No strong opinion" "No strong opinion"  "No strong opinion" 2    
# [3,] "Unfavorable"       "Unfavorable"        "Unfavorable"       1    
# [4,] "Very favorable"    "Very favorable"     "Very favorable"    4    
# [5,] "Very unfavorable"  "Very unfavorable"   "Very unfavorable"   0   
# [6,] NA                  NA                   NA                

issue_names2 = names(select(df, Feminism.SQ001.:HumanBiodiversity.SQ001.))
for (name in issue_names2) {
  temp = rep(NA, length(df[[name]]))
  df[[name]] = as.numeric(df[[name]])
  temp[df[[name]] == 1] = 3
  temp[df[[name]] == 2] = 2
  temp[df[[name]] == 3] = 1
  temp[df[[name]] == 4] = 4
  temp[df[[name]] == 5] = 0
  df[[name]] = temp
} 



summary(select(df, Feminism.SQ001.:HumanBiodiversity.SQ001.))

tmp = select(df, BasicIncome.SQ001.)
summary(tmp)
# [1,] "No strong opinion" 2
# [2,] "Oppose"           1
# [3,] "Strongly oppose"  0
# [4,] "Strongly support" 4
# [5,] "Support"          3
# [6,] NA
name = "BasicIncome.SQ001."
  temp = rep(NA, length(df[[name]]))
  df[[name]] = as.numeric(df[[name]])
  temp[df[[name]] == 1] = 2
  temp[df[[name]] == 2] = 1
  temp[df[[name]] == 3] = 0
  temp[df[[name]] == 4] = 4
  temp[df[[name]] == 5] = 3
df[[name]] = temp

sapply(names(tmp), function(x) {
  levels(tmp[[x]])
  #levels(tmp[x])
  })

library(psych)
limited_df = select(df, -Age, -Income, -IncomeCharityPortion, -XriskCharity)
limited_df = limited_df[sapply(limited_df, is.numeric)]

write.csv(limited_df, file = "clean_data.csv")

cormatrix = cor(limited_df, use="pairwise.complete.obs")
for (i in 1:ncol(cormatrix)) {
  cormatrix[is.na(cormatrix[,i]),i] = 0
}

# screen plot for Eigen values
VSS.scree(cormatrix)
nfa = 15

#fa(vars, nfactors=3, rotate="varimax")
factors = fa(cormatrix, covar=TRUE, rotate="oblimin", nfactors = 15)

factors$loadings

loadings = as.data.frame(factors$loadings[,1:nfa])

# Look at the Top 10 questions associated with each factor.
n_qs = 10

fact_top = lapply(loadings, function(col) { 
  # indices of largest coefficients
  top_indices = order(abs(dena(col)), decreasing = TRUE)[1:10]
  results = rownames(loadings)[top_indices]
  return( cbind(results, round(col[top_indices],3)) )
})
fact_top

lapply(loadings, str)
# MR6: correlated with Having Read Pact, Twig, Synthesis, Ra, My Little Pony, Significant Digits, Three Worlds Collide
#   [Reading fan/fic]
# MR8: Parcipation in EA community, reading Givewell, EA Identity, Donations, Nate Soares blog, Luke's blog, Agenty Duck, EY's FB, Rationalist FB
#   [EA]
# MR9: read Fire upon the deep, The diamond age, Accelerando, Consider Phlebas, NOT a student, roger williams, ender's game, doing for-profit work, married
#   [Establishedness/Scifi]
# MR3: probable anti-aging escape velocity, cryonics works, many worlds true, agree with miri's mission, simulation universe, miri effectiveness,
#     NOT human race survives 2100, MLP: friendship is optimal, Unemployed
# MR4: Unit of caring, Rationalist Tumblr, Thing of Things, NOT Less Wrong, NOT Miri Effectiveness, Hotel Concierge, Autism Spectrum Disorder, NOT MIRIMission, AnxietyDisorder, NOT LessWrong  
#   [Rationalist Tumblr]
# MR10: Community Too Tolerant of Neoreaction, Community Too Tolerant of Creepy People, Community Too Tolerant of Trolls/Cranks, LW Too Much Jargon, LW Overreliance on the Sequences, 
#   Community Failure To Cite Sources, Community Too Tolerant of Laziness, LW Focus on AI, Community Decline from Dunning-Kruger, Community Literally a Cult
#   [LessWrong sucks]
# MR12: View From Hell, Xenosystems, Ribbon Farm, Hotel concierge, Last Psychiatrist, Bayesed And Confused, Siderea, Luke's blog, Gwern
#   [Cynicism?]
# MR7: Pro Social Justice, Pro Feminism, Believes Global Warming, Anti Human Biodiversity, 
#   Pro Basic Income, Concerned End of Work means Rich Take All, Pro Choice, Not Concerned End of Work -> Idle, Not Xenosystems, IQAge Young?
#   [Progressive-ish]
# MR5: Believes supernatural, believes in creator, believes a religion may be correct, wouldn't genetically modify children, end of work bad,
#   married, young IQ test?
#   [Religious Bent]
# MR15: Reads SlateStar, thinks community no new good content, member SlateStarCodex, did previous survey, 
#   Gwern, Overcoming Bias, thinks community no new content, NOT read Synthesis, read Significant Digits, reads Hacker News
#   [Old crowd?]
# MR13: Thinks community decline from newcomers, self-employed, reads agenty duck, community too tolerant of progressivism, community too autistic,
#   LessWrong member, not ADHD, changed personal health to see singularity, not bipolar
#   [???]
# MR1: High SAT2, High SAT, close automation ends employment year, high IQ, NOT a homemaker, singularity close
# MR2: Overachiever
# MR14: reddit?
# MR11: substance use worries

factnames = c("ReadFanfic", "EA", "EstablishedSciFi", "LifeExtension", "RationalistTumblr", "LesswrongSucks","Cynicism", "Progressive","ReligiousBent", "OldGuard",
  "Xenophobic", "Overachiever1", "Overachiever2", "Redditor", "Druggie")

# impute with column mean
for (i in seq(limited_df)) {
  if (is.numeric(limited_df[[i]])) {
    log_vec = is.na(limited_df[[i]])
    limited_df[log_vec, i] = mean(limited_df[[i]], na.rm=TRUE)
  }
}

head(limited_df$IQ)
predictions = predict(factors, limited_df)

# Form a new df where numeric features have been replaced by factors
factor_df = as.data.frame(predictions)
colnames(factor_df) = factnames

### Nonlinear Classification ####

# Pick a categorical variable to predict: CFAROpinion, Negative/Mostly Negative = 1, Everything Else = 0
# take df$CFARopinion -> 0 1s
# cbind that to factor_df
# NAs? get rid of those rows

# create column for predicting positive/negative CFARopinion
silenced = df$CFAROpinion
levels(silenced) = levels(silenced)[-length(silenced)]
sum(is.na(silenced)) # 359

name = "CFAROpinion"
temp = rep(NA, length(df[[name]]))
temp[df[[name]] == "Mostly Negative"] = 1
temp[df[[name]] == "Mostly Positive"] = 0
temp[df[[name]] == "Negative"] = 1
temp[df[[name]] == "Positive"] = 0
temp[df[[name]] == "No strong opinion"] = 0
factor_df$CFARnegative = temp

sum(is.na(factor_df$CFARnegative)) # 359
sum(factor_df$CFARnegative, na.rm=TRUE)

log_vec = is.na(factor_df$CFARnegative)
factor_df$CFARnegative[log_vec] = sample(factor_df$CFARnegative[!log_vec], sum(log_vec), replace=TRUE)

factor_df$CFARnegative = factor(factor_df$CFARnegative)

# Train random forests
library("randomForest")
p = ncol(factor_df) - 1
sqrt_forest = randomForest(CFARnegative ~ ., factor_df, mtry = floor(sqrt(p)))
#plot(sqrt_forest)

# Actually do a prediction, and then calculate the error
rmse = function(x, y) sqrt(mean((x-y)^2))
predictions = predict(sqrt_forest)

#probs = sqrt_forest$votes[,1]
probs = sqrt_forest$votes[,2]
r = roc(factor_df$CFARnegative, probs)
plot(r) 

limited_df_cfar = select(df, -CFAROpinion, -IncomeCharityPortion, -XriskCharity)


for (i in seq(limited_df_cfar)) {
  if (is.numeric(limited_df_cfar[[i]])) {
    log_vec = is.na(limited_df_cfar[[i]])
    limited_df_cfar[log_vec, i] = mean(limited_df_cfar[[i]], na.rm=TRUE)
  } else {
    log_vec = is.na(limited_df_cfar[[i]])
    limited_df_cfar[log_vec,i] = sample(limited_df_cfar[!log_vec,i], sum(log_vec), replace=TRUE)
  }
}
length(limited_df_cfar)
limited_df_cfar = select(limited_df_cfar, -contains("CFAR"))
limited_df_cfar$CFARnegative = factor_df$CFARnegative

p = ncol(limited_df_cfar) - 1
forest = randomForest(CFARnegative ~ ., limited_df_cfar, mtry = floor(sqrt(p)))

r = roc(limited_df_cfar$CFARnegative, forest$votes[,2])
plot(r)

# dummy factor variables
library("dummies")
dummied_cfar_df = dummy.data.frame(limited_df_cfar)
dummied_cfar_df[["CFARnegative0"]] = NULL

library(glmnet)
# Logistic regression
features = scale(select(dummied_cfar_df, -CFARnegative1))
target = dummied_cfar_df$CFARnegative1
fit = cv.glmnet(features, target, family = "binomial")

# Examine the coefficients

coefficients = coef(fit$glmnet.fit, s=fit$lambda.min)
nonzerocoefs = coefficients[abs(coefficients) > 0]
names(nonzerocoefs) = rownames(coefficients)[abs(as.vector(coefficients)) > 0]

library(corrplot)
corrplot(as.matrix(nonzerocoefs[-1]), is.corr=FALSE)  # MIRIEffectiveness!
forest$importance[order(forest$importance, decreasing=TRUE), ]  # MIRIEffectiveness!
# Similarities: MIRI, CommunityIssuesNow.17
# referredby is high in forest but not logistic
# Successor philosophy 2 Less is in logistic high but not forest

colnames(features) = make.names(colnames(features), unique=TRUE)
library(caret)

target_factor = factor(target)
levels(target_factor) = c('m0', 'm1')

control = trainControl(method="repeatedcv", number=3, repeats=1, verboseIter=TRUE, classProbs = TRUE, summaryFunction = twoClassSummary)
glmfit = train(x = features, y = target_factor, tuneLength = 10, method = "glmnet", metric = "ROC", trControl = control)
glmpredictions = predict(glmfit$finalModel, features, s=glmfit$bestTune$lambda)

cor(df$MIRIEffectiveness, df$CFAROpinion == "Positive" | df$CFAROpinion == "Mostly Positive", use="pairwise.complete.obs")
roc(target, glmpredictions, plot=TRUE)
glmnetcoefs = coef(glmfit$finalModel, s= glmfit$bestTune$lambda)
nonzerocoefs = glmnetcoefs[abs(glmnetcoefs) > 0]
names(nonzerocoefs) = rownames(glmnetcoefs)[abs(as.vector(glmnetcoefs)) > 0]

corrplot(as.matrix(nonzerocoefs[-1]), is.corr=FALSE, cl.pos="n")
# disagree with miri
# think LW is a cult
# not coming back to lesswrong
# negative opinion of EA
# think focus on AI is a problem with LW

rffit = train(x = features, y = target_factor, tuneLength = 10, method = "rf", trControl = control)
# 7 features, ROC: 0.79

# TODO: do this with factors
