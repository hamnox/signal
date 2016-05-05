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

