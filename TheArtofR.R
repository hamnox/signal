examsquiz <- read.table("ExamsQuiz.txt", header=FALSE)
class(examsquiz)
head(examsquiz)
lma <- lm(examsquiz[,2] ~ examsquiz[,1])
# equivalent to
# lma <- lm(examsquiz$V2 ~ examsquiz$V1)
attributes(lma)

lmb <- lm(examsquiz[,2] ~ examsquiz[,1] + examsquiz[,3])

# don't write for-loops like this, what if 1:0?
first1 <- function(x) {
  for (i in 1:length(x)) {
    if (x[i] == 1) break #break out of loop
  }
  return(i)
}
# this should use seq() instead, as a safe alternative

# Arrays and matrices are vectors, with extra class attributes


