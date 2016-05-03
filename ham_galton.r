install.packages("HistData")
install.packages("dplyr")
library("HistData")
library("dplyr")
library("ggplot2")
library("GGally")

df = na.exclude(GaltonFamilies)

View(df)

# mid-parent height, calculated as (father + 1.08*mother)/2
mutate(df, gender = as.numeric(gender == "female")) %>%
  select(midparentHeight, gender) %>%
  ggpairs()

names(df)
linear_fit = lm(df$childHeight ~ df$father + df$mother +
                  df$children + df$gender + df$midparentHeight)
  # can't remember how to do linear fit
summary(linear_fit)

mutate(df, gender = as.numeric(gender == "female")) %>%
  group_by(family) %>%
  mutate(avgHeight=mean(childHeight)) -> by_family


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


p1 <- ggplot(by_family, aes(y=avgHeight, x=father)) + geom_point()
p2 <- ggplot(by_family, aes(y=avgHeight, x=mother)) + geom_point()
p3 <- ggplot(by_family, aes(y=avgHeight, x=midparentHeight)) + geom_point()

View(by_family)
by_family
multiplot(p1, p2, p3, cols=3)


