install.packages('tawny')
library(tawny)
a = 1:10
a > 6
b = sample(1:10)
b[order(b)]
order(b)
names(a) = strsplit('abcdefghij','',fixed = TRUE)[[1]]
a
a['c']
t(1:4)

hmm = matrix(rnorm(120), ncol = 6)
colnames(hmm) = c("C", "F", "T", "A", "D", "K")
hmm['A']
hmm[,'A']
hmm[,c("A", "F")]

f <- function(x) 3 * x + 2
f(-5:5)

f <- function(x,y=3) (x-5)^2 + (y+2)^2
f(y=3,x=4)
f(4)

# WHAT! Ellipsis arguments are passed wholesale?!
f <- function(x, ...) plot(x, ...)
f(rnorm(100), main="100 Random Values")

h <- getPortfolioReturns(c("AAPL","XOM","KO","F","Gs"), 100)
apply(h, 2, sd)

counter <- function(start=0) {
  x <- start
  function() { x <<- x + 1; x }
}

f <- counter(4)
f()
