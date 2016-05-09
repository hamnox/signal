examsquiz <- read.table("ExamsQuiz.txt", header=FALSE)
class(examsquiz)
head(examsquiz)
lma <- lm(examsquiz[,2] ~ examsquiz[,1])
# equivalent to
# lma <- lm(examsquiz$V2 ~ examsquiz$V1)
attributes(lma)

lmb <- lm(examsquiz[,2] ~ examsquiz[,1] + examsquiz[,3])
