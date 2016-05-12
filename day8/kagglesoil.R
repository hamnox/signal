# https://www.kaggle.com/c/afsis-soil-properties/download/sample_submission.csv

getwd()
soil_df = read.csv("day8/training.csv")
# dplyr has starts_with("characters"), ends_with, contains OMG SO ANGRY


# so imaginining two variables that are interfering with each other...
# var1, var2
# if you predict one using the other...
# the mean/median of the RSS could be far off zero
# DARN what I really just need to do is to simulate these happening.
  # If you predict one using the other AND THE OUTCOME VARIABLE
  # then you expect to see a significant amnt of the variance go away?


# 1, 2, 3 and outcome
# outcome ~ 1
# outcome ~ 2
# outcome ~ 3

# outcome ~ independent additive effects (pos or neg)
# outcome ~ any threshold over
# outcome ~ either 2 threshold over or 1 independent additive effect
# outcome ~ either 2 threshold over and 1 independent additive effect
# outcome ~ both threshold over or 1 independent additive effect
# outcome ~ both threshold over and 1 independent additive effect
# outcome ~ 1 threshold over or 2 independent additive effects
# outcome ~ 2 correlated additive + 1 additive effect
