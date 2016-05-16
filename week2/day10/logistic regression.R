
library("foreign")
# install.packages("caret")
library("caret")
# install.packages("dummies")
library("dummies")
library("dplyr")
library("corrplot")
library("pROC")

# --------------------------------------------------------------------
# setups -------------------------------------------------------------
# --------------------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# initialization
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# documentation here: https://github.com/signaldatascience/R-curriculum/blob/master/pdfs/week2/day5/nes-glossary.txt
elections_df <- read.dta("day10/elections.dta")



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

get_scaled_matrices = function(dataframe, resultscolname, center = TRUE, scale = TRUE) {
  # arrgh have to remove all the single-value columns for some shit reason.
  # they turn into nans.
  # add that automatically later
  x = as.matrix(dataframe[-match(resultscolname, names(dataframe))])
  x = scale(x, center, scale)
  
  # Y IS ASSUMED TO BE A FACTOR, FOR LOG ODDS PREDICTIONS
  y = as.factor(dataframe[[resultscolname]])
  return(list(x = x, y = y))
}

get_caret_fits = function(dataframe, resultscolname) {
  # temp
  # dataframe = data_df
  # resultscolname = "presvote_republican"

  scaled_matrices = get_scaled_matrices(dataframe, resultscolname)

  # because I got a warning about factor levels being invalid variable names
  print("scaling variables")
  levels(scaled_matrices[["y"]]) = c("no", "yes")
  
  
  # alphas 0 to 1, 10 gradient
  # lambdas 0 to 0.25, 50 gradient
  param_grid = expand.grid(.alpha = 1:10 * 0.1,
                           .lambda = seq(0, 0.25, length.out=25))
  # classProbs = TRUE makes it be logodds # number = number of folds
  # repeats = number of epochs # summaryFunction = twoClassSummary for binary prediction
    # multiClassSummary for multiple class prediction
  
  control = trainControl(method="repeatedcv",
                         number=5,
                         classProbs=TRUE,
                         repeats=1,
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
set.seed(0)
elections_df %>% select(year, age:religion, vote, presvote) %>%
  filter(year %% 4 == 0) -> df

df <- impute_distribution(df, names(df)[-(1:2)])
df <- impute_average(df, names(df)[1:2])
# head(df)

# for (name in get_factor_columns(df, "name")) {
#   print(name)
#   print(levels(df[[name]]))
# }

levels(df[["gender"]]) = c("na", "male", "female")
levels(df[["race"]]) = c("white", "black", "asian", "native_am", "hispanic", "other", "na")
levels(df[["educ1"]]) = c("na", "gradeschool", "highschool",
                          "somecollege", "college")
levels(df[["urban"]]) = c("na", "central_cities", "suburban", "rural")
levels(df[["region"]]) = c("na", "northeast", "northcentral",
                           "south", "west")
levels(df[["income"]]) = c("na", "0_16percentile", "17_33percentile",
                           "34_67percentile", "68_95percentile",
                           "96_100percentile")
levels(df[["occup1"]]) = c("na", "professional_managerial",
                           "clerical_sales", "service_workers",
                           "laborers", "farm_laborers", "homemakers")
levels(df[["union"]])= c("na", "yes", "no")
levels(df[["religion"]]) = c("na", "protestant", "catholic",
                             "jewish", "other_none")
levels(df[["vote"]]) = c("na", "no", "yes")
levels(df[["presvote"]]) = c("na", "democrat", "republican", "third party")

# 13 = presvote
data_df = dummy.data.frame(df, names=names(df)[-c(1,2)], sep="_")
remove(df)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# WISHLIST
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# subset data
  # party support by year
  # get party support by year, restrict to nonvoters


# --------------------------------------------------------------------
# --------------------------------------------------------------------
# --------------------------------------------------------------------

# I'm going to try something different today:
# make a section outline for all the parts I need, THEN go back and fill
# in the pieces. If I spend longer than 10 minutes on any part, outline
# it some more.

# If it's properly encapsulated like most of my non-R code is, then I
# can go back and write in optimizations later IFF I have time not
# running off into tangents as soon as I think them up.


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# main run
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# visualize data

mosaicplot(table(df$income, df$presvote))
# curious... more republicans tend to have higher income.
mosaicplot(table(df$year, df$presvote))
# for some reason there's a lot of data on 1972
# looks kinda cyclical
mosaicplot(table(df$race, df$presvote)) # very little data on....
# I really wish I could get the distribution stuff to work right, rather than just using sample

names(data_df)
# [1] "year"                           "age"                            "gender_male"                   
# [4] "gender_female"                  "race_white"                     "race_black"                    
# [7] "race_asian"                     "race_native_am"                 "race_hispanic"                 
# [10] "race_other"                     "educ1_gradeschool"              "educ1_highschool"              
# [13] "educ1_somecollege"              "educ1_college"                  "urban_central_cities"          
# [16] "urban_suburban"                 "urban_rural"                    "region_northeast"              
# [19] "region_northcentral"            "region_south"                   "region_west"                   
# [22] "income_0_16percentile"          "income_17_33percentile"         "income_34_67percentile"        
# [25] "income_68_95percentile"         "income_96_100percentile"        "occup1_professional_managerial"
# [28] "occup1_clerical_sales"          "occup1_service_workers"         "occup1_laborers"               
# [31] "occup1_farm_laborers"           "occup1_homemakers"              "union_yes"                     
# [34] "union_no"                       "religion_protestant"            "religion_catholic"             
# [37] "religion_jewish"                "religion_other_none"            "vote_no"                       
# [40] "vote_yes"                       "presvote_democrat"              "presvote_republican"           
# [43] "presvote_third party"    


# predict support for George H.W. Brush in 1992, restricting to people who actually voted
#------------------------------------------
data_df %>% filter(vote_yes == 1) %>% filter(year == 1992) %>%
  select(-year, -race_other, -(vote_no:vote_yes),-presvote_democrat, -`presvote_third party`) -> george_bush_df


  
gb_fits_stuff <- get_caret_fits(george_bush_df, "presvote_republican")
gb_model = gb_fits_stuff[["model"]]
gb_fits_stuff[["plotfit"]]
qplot(x = george_bush_df[["presvote_republican"]],
      y = predict(gb_model, newx=get_scaled_matrices(george_bush_df, "presvote_republican")[["x"]],
              s = gb_model$lambdaOpt))
gb_coefficients = coef(gb_model, s = gb_model$lambdaOpt)


corrplot::corrplot(as.matrix(gb_coefficients), is.corr = FALSE)
# anti-predictd by being black. mostly an intercept thing
# white, asian, college, protestant are for
# central-cities, religion other/none is against
# would be nice if I felt like I had time for interaction testing. I still
  # don't understand the principled way to do that.

# predict party support for different years, look at how the coefficients of those predictors change over time
#------------------------------------------
data_df %>% filter(presvote_republican + presvote_democrat == 1) %>%
  select(-`presvote_third party`, -presvote_republican) -> support_df

year_coefficients = lapply(unique(support_df$year), function(votingyear) {
  print(paste("year", votingyear))
  year_df = filter(support_df, year == votingyear)
  for (name in names(year_df)) {
    if (length(unique(year_df[[name]])) < 2) {
      year_df = year_df[-match(name, names(year_df))]
    }
  }
  gb_fits_stuff <- get_caret_fits(year_df, "presvote_democrat")
  coefficients = coef(gb_fits_stuff$model, s=gb_fits_stuff$model$lambdaOpt)
  return(coefficients)
})
names(year_coefficients) = unique(support_df$year)

year_coef_matrix = t(sapply(names(select(support_df, -year, -presvote_democrat)), function(name) {
  sapply(year_coefficients, function(m) {
    if (name %in% rownames(m)) {
      return(m[name, 1])
    } else {
      return(0)
    }
  })
}))
colnames(year_coef_matrix) = unique(support_df$year)

year_coef_matrix[rowSums(abs(year_coef_matrix)) > 0.5,] -> summarized_year_coef_matrix

corrplot(summarized_year_coef_matrix, is.corr=FALSE)
  # race became important starting ~ 1964. being black was a major democratic determinant
    # in 1968, 1992, 2000
  # grade school education leans mildly democrat... this surprises me and I wonder if
    # this confounds with race and income
  # negative determinant if you were region_northeast in 1956, but it became slightly
    # democrat towards 1996
  # region_south leaned democrat in 1952-1956, more solidly republican in 2000
  # the top 5 percentile income have stayed slightly republican
  # central cities all lean very slightly democrat around 1980-1996
  # unions have stayed democrat
  # I'm thinking I should maybe go back and remove the na answers.
  # protestants favored HUGELY republican 1952-1972, some resurgance again in 1992
  # jewish democrat correlation around 1956, died down, then came back 2000



# for voters who didn't vote, predict how they would have voted. aggregated by election year, how do they change over time?
#------------------------------------------
data_df %>% filter(vote_yes == 1, `presvote_third party` == 0) %>%
  select(-(vote_no:vote_yes),-presvote_republican, -`presvote_third party`) -> voters_df

data_df %>% filter(vote_yes == 0, `presvote_third party` == 0) %>%
  select(-(vote_no:vote_yes),-presvote_republican, -`presvote_third party`) -> nonvoters_df
# incidentally, I should have removed the people who voted for presvote_third party
# or redone the caret function to do multiclass on the general presvote factor
# I'm not going to do so now

voter_stuff = get_caret_fits(voters_df, "presvote_democrat")
# reference:  list(model, predictions, xscales list(center, scale))

corrplot(as.matrix(coef(voter_stuff$model, s=voter_stuff$model$lambdaOpt)), is.corr = FALSE)
giveny = voters_df$presvote_democrat
roc(giveny, voter_stuff$predictions, plot=TRUE) # not a very impressive graph
cor(giveny, voter_stuff$predictions) # == 0.3382187... R2 of .11439 yeah I'm not impressed

cor(data_df, data_df) -> silenced
temp = silenced * (diag(nrow(silenced), x=(-1)) + 1)
sdmin = mean(abs(temp)) + 2*sd(abs(temp))
pull = sapply(colnames(silenced), function(i) {
  col_log = abs(temp[,i]) > sdmin
  new = temp[col_log, i]
  names(new) = rownames(temp)[col_log]
  return(new)
})

corrplot(temp)
# unsurprisingly, the most obviously correlated/not are the indicator variables.
# I really need to go back and make sure unused indicator variables are dropped
# managerial is correlated with college, age with gradeschool
# homemakers are correlated with womenhood

# remove pointless vars
voters_df %>% mutate(homemaker_male = gender_male * occup1_homemakers) %>%
  select(-gender_female, -union_no) -> voters_df2
nonvoters_df %>% mutate(homemaker_male = gender_male * occup1_homemakers) %>%
  select(-gender_female, -union_no) -> nonvoters_df2

voter_stuff2 = get_caret_fits(voters_df2, "presvote_democrat")

scaled_nonvoters = get_scaled_matrices(nonvoters_df2, "presvote_democrat", center = voter_stuff2$xscales[["center"]],
                                       scale = voter_stuff2$xscales[["scale"]])[["x"]]
nonvoters_df2$predicted = predict(voter_stuff2$model,
                               newx = scaled_nonvoters,
                               s = voter_stuff2$model$lambdaOpt)[,1]

# coefficients didn't appear to change based on the homemaker_male thing.. maleness became
# more signicant bc. female disappeared of course, and union more democrat
corrplot(as.matrix(coef(voter_stuff2$model, s=voter_stuff2$model$lambdaOpt)), is.corr = FALSE)
nonvoters_df2 %>% group_by(year) %>% summarise(actualvals = mean(presvote_democrat),
                                               predictedvals = as.vector(mean(predicted)),
                                               actual25 = mean(presvote_democrat) - sd(presvote_democrat),
                                               actual75 = mean(presvote_democrat) + sd(presvote_democrat),
                                               predicted25 = mean(predicted) - sd(predicted),
                                               predicted75 = mean(predicted) + sd(predicted)) -> plotdf
(ggplot(plotdf, aes(x=year, group=year)) +
  geom_pointrange(aes(y = actualvals, ymin = actual25, ymax=actual75), position=position_nudge(x=-.3)) + 
  geom_pointrange(aes(y = predictedvals, ymin = predicted25, ymax=predicted75),
                  color="blue", fill="blue", position=position_nudge(x=.3))
  )

# wweeiiirdd....the balance of actual would-have-voted stays the same over the years,
# but we predict closer to democrat. Not so surprising, considering year itself had a coefficient.
# but it's consistently *lower* than it should be on average.

(ggplot(nonvoters_df2, aes(x=predicted, y=presvote_democrat, group = year, color=year))
  + geom_jitter() + geom_smooth(method = "glm", method.args = list(family = "binomial"),  se=FALSE)
) # wow. plenty of years I'm anti-predicted.

# personal interest: can you predict who will actually vote?

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~main run end
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# notes:
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(
  paste(
  "USE mosaicplot(table(stuff)) for visualizing stuff",
  "Err: used typeof() to try to get is.factor"
       
       
       
       , sep="\n")
 )
function(miscwork) {
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# misc work
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# trying to figure out which years had a presidential election
names(elections_df)
length(elections_df$presvote) # 41498
sum(!is.na(elections_df$presvote)) # 15539
head(unique(cbind(elections_df$year, elections_df$presvote)),100)
min(elections_df$year) # 1948, 2002
max(elections_df$year)


# trying to figure out if I can use a table to make my distribution work
silenced = table(df[["year"]])
as.integer(names(silenced)[1]) # == 1948, so that will work fine
silenced[order(silenced, decreasing=TRUE)]


#  THIS I CANNOT MAKE WORK I'M JUST GONNA DO SAMPLE
distribution = table(colvalues)
distribution = distribution[order(distribution, decreasing = TRUE)]
distribution = ceiling(distribution * sum(NAs_L) / totalrows)
checksum = sum(NAs_L) - sum(distribution)
while (checksum != 0) {
  ordered = seq(distribution)
  if (checksum < 0) {
    ordered = ordered[order(ordered)]
  }
  for (i in ordered) {
    distribution[i] = distribution[i] + checksum * round(distribution[i] / sum(distribution))
    if ((sum(NAs_L) - sum(distribution)) == 0) {
      break
    }
  }
  print(checksum)
}

# testing c() behavior
silenced = c()
silenced = c(silenced, "A")
silenced # == "A"


# testing dummy's na behavior
silenced = c( NA, "l", NA)
dummy(silenced)
dummy(silenced,sep=":")

# debugging get_caret_fits in practice
silenced = get_scaled_matrices(george_bush_df)

# figuring out how to rowbind matrices/vectors by rowname
head(year_coefficients[[1]])
cbind2(1:3, 4)
cbind2(1:2, m)
head(year_coefficients[[1]]["gender_male",1])
head(year_coefficients[[1]]["race_white",1])
"a" %in% c("b", "a", "c", "a") # TRUE
"a" %in% c("b", "c", "c", "d") # FALSE
sapply(year_coefficients, class)
sapply(year_coefficients, rownames)

}
