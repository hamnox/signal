### UN INFANT MORTALITY DATA ###

# Write code here to load packages and data
install.packages(c("car", "Ecdat", "HistData", "ggplot2", "dplyr", "GGally"))
library("car")
library("ggplot2")
library("GGally")
df = UN

# Write code here to calculate correlations
cordata = cor(df, use = "na.or.complete")
  # use = "complete.obs" or "na.or.complete" to ignore incomplete cases
# round(cordata * 100)
cor2 <- function(dataframe) {
  return(round(cor(df, use = "na.or.complete") * 100))
}
cor2(df)
# Write code here to make a new dataframe with incomplete rows omitted
df2 = na.omit(df)
df2
# Write code here to examine the distribution of the data
ggpairs(df2)
  # the distributions are very skewed, concentrated in a peak on the left

# Write code here to take the log transformation of the data
ldf = log(df2)

# Write code here to examine the distribution of the log-transformed data
ggpairs(ldf)
  # much better as a transformed data... more of a line, closer to natural distribution

# Calculate linear fit of infant mortality vs. GDP
linear_fit = lm(infant.mortality ~ gdp, df)
summary(linear_fit)
# Calculate linear fit of log(infant mortality) vs. log(GDP)
loglog_fit = lm(infant.mortality ~ gdp, ldf)
summary(loglog_fit)

# Plot the linear fit of infant mortality vs. GDP
ggplot(df, aes(gdp, infant.mortality)) + geom_point() + geom_smooth(method = "lm")
ggplot(ldf, aes(gdp, infant.mortality)) + geom_point() + geom_smooth(method = "lm")
  # geom_smooth defaults to loess method if <1000 observations, otherwise gam
  # the linear model is pretty good

# Plot of linear fit residuals
qplot(ldf$gdp, linear_fit$residuals)
  # yeah, seems there's heteroscedasticity
qplot(ldf$infant.mortality, linear_fit$fitted)

# Plot of linear qplot(ldf$gdp, linear_fit$residuals) residuals after log transformation of GDP and infant mortality
qplot(df2$gdp, df2$infant.mortality - exp(fitted(loglog_fit)))
  # no, not really an improvement
  # I suppose heteroscedasticity violates the assumptions of linear fitting
  # it says assumptions of linear regression is that variance is constant