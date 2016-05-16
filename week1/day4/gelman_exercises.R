install.packages("foreign")
library("foreign")
iq.data = read.dta("/home/derrick/signal/day4/child.iq/child.iq.dta")
install.packages("arm")
select = dplyr::select
library("arm")
library("ggplot2")
library("dplyr")

# 3.4.a
test_momage = lm(iq.data$ppvt ~ iq.data$momage)
summary(test_momage)

p = ggplot(iq.data, aes(x=momage, y=ppvt))
p + geom_point() + geom_smooth(method="lm")
summary(iq.data)

# have your kid at age 29
# each year increase in mother's age, you expect a 0.8403 increase in IQ
# caution: the range of ages in our data set are only from 17-29
# it's not even causational, we should interpret this as subgroups
# moms who wait longer tend to increase their child's IQ for some reason
# adjusted R^2 is 0.01, so not that much of the data is explained

# 3.4.b

test_momage_edu = lm(iq.data$ppvt ~ iq.data$momage + iq.data$educ_cat)
summary(test_momage_edu)

# yes, because it seems mother's education category is much more relevant
# otoh, it seems like these predictors aren't independent
# adjusted R^2 is 0.04

# 3.4.c
hs_grad = as.numeric(iq.data$educ_cat >= 2)
grad.iq.data = cbind(iq.data, hs_grad)

test_momage_grad = lm(grad.iq.data$ppvt ~ grad.iq.data$momage 
                      + grad.iq.data$hs_grad)
summary(test_momage_grad)

# Slightly better R^2 but more difficult to interpret
test_momage_grad_int = lm(grad.iq.data$ppvt ~ grad.iq.data$momage*grad.iq.data$hs_grad)
summary(test_momage_grad_int)

graduated = dplyr::filter(grad.iq.data, hs_grad == 1)
p1 = ggplot(graduated, aes(x=momage, y=ppvt))
not_graduated = dplyr::filter(grad.iq.data, hs_grad == 0)
p2 = ggplot(not_graduated, aes(x=momage, y=ppvt))
p1 = p1 + geom_point() + geom_smooth(method=lm)
p2 = p2 + geom_point() + geom_smooth(method=lm)

plot2 = ggplot(NULL)

plot2 + geom_point(data=graduated, aes(x=momage, y=ppvt, color = "grey")) + geom_point(data=not_graduated, aes(x=momage, y=ppvt)) + geom_smooth(data=graduated,method="lm", aes(x=momage, y=ppvt, color="blue")) + geom_smooth(data=not_graduated,method="lm", aes(x=momage, y=ppvt, color="augh"))

multiplot(p1, p2)

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

training = iq.data[1:200,]
dim(training)
test = iq.data[201:400,]
dim(test)

# You know, edu_cat should probably be a factor.
train_fit = lm(training$ppvt ~ training$momage + training$educ_cat)
?predict

predictIQ = predict(train_fit, data=test)
predictions = cbind(test, predictIQ)
predictions
plot(predictions$predictIQ, predictions$ppvt)

# residual plot
hist(predictions$predictIQ - predictions$ppvt)

# 3.5.a
# Read the data into R, including the variable names (headers)
beauty.data <- read.csv ("/home/derrick/signal/day4/beauty/ProfEvaltnsBeautyPublic.csv")

n = names(beauty.data)
beauty.data$huey = beauty.data$btystdave
beauty.data = beauty.data[-grep("class|beauty|bty",n)]
str(beauty.data)

# Rename the two variables for convenience
beauty <- beauty.data$btystdave
eval <- beauty.data$courseevaluation

beauty_vs_eval = data.frame(beauty, eval)

fit = lm(beauty_vs_eval$eval ~ beauty_vs_eval$beauty)
p = ggplot(beauty_vs_eval, aes(x=eval, y=beauty))
p + geom_point() + geom_smooth(method="lm")
summary(fit)
# pretty significant, but not much effect
plot(fit)
# looks reasonable

# 3.5.b
names(beauty.data)
attach(beauty.data)
fit = lm(eval ~ female)
# Everything to the right of ~ is a predictor and an input
fit = lm(eval ~ beauty + female + minority + age + nonenglish + onecredit)
summary(fit)
# interaction term, additional predictors of the interaction terms
fit = lm(eval ~ beauty*female + minority*female + age*female + nonenglish + onecredit)
summary(fit)

# stepwise

library(dplyr)
names(beauty.data)
head(beauty.data)

min.model = lm(courseevaluation ~ 1, beauty.data)
biggest = formula(lm(courseevaluation~.-profevaluation,beauty.data))
model = step(min.model, direction='forward', scope=biggest)
model
