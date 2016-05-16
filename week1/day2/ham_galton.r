#install.packages("HistData")
#install.packages("dplyr")
library("HistData")
library("dplyr")
library("ggplot2")
library("GGally")

df = na.exclude(GaltonFamilies)

View(df)

# mid-parent height, calculated as (father + 1.08*mother)/2
mutate(df, gender = as.numeric(gender == "female")) %>%
  select(midparentHeight, childHeight) %>%
  ggpairs()

names(df)
linear_fit = lm(df$childHeight ~ df$father + df$mother +
                  df$children + df$gender + df$midparentHeight)
  # can't remember how to do linear fit
summary(linear_fit)$adj.r.squared

mutate(df, gender = as.numeric(gender == "male")) %>%
  group_by(family) %>%
  summarize(father = min(father),
            mother = min(mother),
            midparentHeight = min(midparentHeight),
            children = min(children),
            avgSiblingsHeight=mean(childHeight),
            avgSiblingsGender=mean(gender),
            siblingHeightVariance = var(childHeight)) -> by_family


# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
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

# http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/


p1 <- ggplot(by_family, aes(y=avgSiblingsHeight, x=father, color=avgSiblingsGender)) + geom_point() + theme(legend.position="none")
p2 <- ggplot(by_family, aes(y=avgSiblingsHeight, x=mother, color=avgSiblingsGender)) + geom_point() + theme(legend.position="none")
p3 <- ggplot(by_family, aes(y=avgSiblingsHeight, x=midparentHeight, color=avgSiblingsGender)) + geom_point() + theme(legend.position="none")
p4 <- ggplot(by_family, aes(y=avgSiblingsHeight, x=children, color=avgSiblingsGender)) + geom_point() + theme(legend.position="none")
multiplot(p1, p2, p3, p4, cols=2)



# gender is the highest thing
# qplot(x, y, color, data, geom)
linear_fit = lm(by_family$avgSiblingsHeight ~ by_family$avgSiblingsGender + by_family$father + by_family$mother)
  summary(linear_fit)$adj.r.squared #.51
# + mom = 0.595

# father most important
linear_fit = lm(by_family$avgSiblingsHeight ~ by_family$father)
  summary(linear_fit)$adj.r.squared #.10
# mom - .06
# midparent - .155
# children - .002

  linear_fit = lm(by_family$children ~ by_family$father)
  summary(linear_fit)$coefficients
  linear_fit = lm(by_family$children ~ by_family$mother)
  summary(linear_fit)$coefficients
  linear_fit = lm(by_family$children ~ by_family$avgSiblingsHeight)
  summary(linear_fit)$coefficients

  # maaaybe the intercept is significant?

ggplot(by_family, aes(x=children)) + geom_histogram(binwidth=1)

# p-value: 5.62e-10
# Adjusted R-squared: 0.0395
child_mother = lm(df$childHeight ~ df$mother)
summary(child_mother)
# p value: 2.2e-16
# Adjusted R-squared: 0.06978
child_father = lm(df$childHeight ~ df$father)
summary(child_father)
# p value: 2.2e-16
# sadly, adjusted R-squared is 0.102
child_midparent = lm(df$childHeight ~ df$midparentHeight)
summary(child_midparent)
# p value: 2.2e-16
# Adjusted R-squared is 0.5132
child_gender = lm(df$childHeight ~ df$gender)
summary(child_gender)

gender_fatherHeight = lm(by_family$avgSiblingsGender ~ by_family$mother)
summary(gender_fatherHeight)

# yeah, so adjusted R-squared here is 0.5934, not bad
child_fatherHeight_gender = lm(df$childHeight ~ df$father + df$gender)
summary(child_fatherHeight_gender)

male_children = filter(df, gender == "male")
female_children = filter(df, gender == "female")

# p-value: 3.838e-13
# Adjusted R-squared:  0.1025 
male_mother = lm(male_children$childHeight ~ male_children$mother)
summary(male_mother)
# p-value: 3.222e-11
# Adjusted R-squared:  0.09111 
female_mother = lm(female_children$childHeight ~ female_children$mother)
summary(female_mother)
# p-value: < 2.2e-16
# Adjusted R-squared:  0.1522
male_father = lm(male_children$childHeight ~ male_children$father)
summary(male_father)
# p-value: < 2.2e-16
# Adjusted R-squared:  0.1817 
female_father = lm(female_children$childHeight ~ female_children$father)
summary(female_father)

female_father_mother = lm(female_children$childHeight ~ female_children$father + female_children$mother)
summary(female_father_mother)
male_father_mother = lm(male_children$childHeight ~ male_children$father + male_children$mother)
summary(male_father_mother)

# theoretically, you could do it. but not with this dataset
# it doesn't differentiate that way, it's decreasing order of height
# also it does boys then girls..

#-------------------------------------------------------
qqnorm(df$midparentHeight)
plt <- qqnorm(df$midparentHeight)
twstd = 2 * sd(df$midparentHeight)
dfmean = mean(df$midparentHeight)
new = df$midparentHeight[(df$midparentHeight > (dfmean - twstd)) & (df$midparentHeight < (dfmean + twstd))]
qqline(new)


mutate(df, gender = as.numeric(gender == "female")) %>%
  group_by(family) %>%
  summarize(father = mean(father),
            mother = mean(mother),
            midparentHeight = mean(midparentHeight),
            children = mean(children)) -> numericdf

cor(numericdf[2:5])

numericdf <- mutate(df, gender = as.numeric(gender == "female"))

cor(numericdf[2:8]) %>%
  mutate(father, replace(father, abs(father) > 0.2, NA),
         mother, replace(mother, abs(mother) > 0.2, NA),
         midparentHeight, replace(midparentHeight, abs(midparentHeight) > 0.2, NA),
         father, replace(father, abs(father) > 0.2, NA)
         ) 

