# spam corpus



library("caret")
# install.packages("dummies")
library("dummies")
library("dplyr")
library("corrplot")

# --------------------------------------------------------------------
# setups -------------------------------------------------------------
# --------------------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# initialization
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

spam_df <- read.csv("day10/spam-emails.csv")
spam_keys_df <- read.csv("day10/spam-emails-key.txt", header = FALSE, sep=" ")