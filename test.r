# assignment
x = 100
x
x <- 100
x

# installing packages

install.packages("packagename")

# change directory
getwd()
setwd("/home/derrick/signal")
getwd()

# types

typeof(123)
typeof(123L)

is.numeric(3)
is.numeric(3.1)
is.numeric(5L)

TRUE
FALSE
T
F
TRUE == FALSE
TRUE & (T | FALSE)
F = T
F & TRUE
F = "zimbabwe"
F

FALSE = TRUE
F = FALSE

# Atomic Vectors
c(1) == 1
is.atomic(c(1, 2, 4))
is.atomic(1)
# no difference wat

c(1, 3, c(4, 5, 6, 7), c(2))
# it just strings them all together

v = c(1, 2, 3)
c(v, 4)
# vectors force consistently silently
c(v, TRUE)

c(5, FALSE, TRUE, 10)
c('asdf', FALSE)
c(TRUE, 'qwerty', 10.10)
as.character(c(5, FALSE, TRUE))
as.character(c(FALSE, TRUE))
mean(c(FALSE, TRUE, TRUE, TRUE, TRUE, FALSE))
length(c(FALSE, TRUE, TRUE, TRUE, TRUE, FALSE))

# 1 0
c(1, FALSE)

# "a" "1"
c("a", 1)

# "1" "a"
c(list(1), "a")
# huh forces both to be lists

# 1L 1L
typeof(c(TRUE, 1L))

x = runif(1)
if (x < 0.5) {
  print(x)
} else {
  print('big')
}

vec = c()
for (n in 1:30) {
  vec = c(vec, paste("label ", n))
}
vec

for (x in rnorm(10)) {
  if (x < 0.5) {
    print(x)
  } else {
    print("big")
  }
}

vec = c()
for (x in 10:100) {
  vec <- c(vec, x^3 + 4 * x^2)
}
sum(vec)

vec = c()
for (i in 1:25) {
  vec = c(vec, (2^i)/i + (3^i)/(i^2))
}
sum(vec)

install.packages("tictoc")
require(tictoc)

x = 1:10000

tail_times = c()
len_times = c()
seq_times = c()
tic()
for (n in 1:1000) {
  # 0.014, 0.007
  tic()
  tail(x)
  t = toc()
  tail_times = c(tail_times, t[[2]] - t[[1]])

  #0.01, 0.004
  tic()
  x[(length(x) - 5):length(x)]
  l = toc()
  len_times = c(len_times, l[[2]] - l[[1]])
  
  #0.009, 0.006
  tic()
  xlen = length(x)
  x[seq(xlen-5, xlen)]
  s = toc()
  seq_times = c(seq_times, s[[2]] - s[[1]])
}

mean(tail_times)
mean(len_times)
mean(seq_times)

vec = c()
for (x in seq(3,6, by=0.1)) {
  vec = c(vec, (exp(x) * cos(x)))
}
vec

for (x in 1:20) {
  for (y in x:20) {
    print(paste(x, y, sep = ", "))
  }
}


# defining functions

collatz = function(n) {
  if ((n %% 2) == 0) {
    result = (n / 2)
  } else {
    result = (3 * n + 1)
  }
  result
}
collatz(3)

previous = 21
for (i in 1:25) {
  previous = collatz(previous)
  print(previous)
}

# hundred integers

values = c()
for (i in 1:100) {
  last = i
  cnt = 0
  while (last != 1) {
    last = collatz(last)
    cnt = cnt + 1
  }
  values = c(values, cnt)
}

hist(values)

count = 0
num_ways <- function(n) {
  for (x in seq.int(1,(n^(1/3)), by=1)) {
    for (y in seq.int(x, n^(1/3), by=1)) {
      if (x^3 + y^3 == n) {
        count = count + 1
      }
    }
  }
  return(count)
}

num_ways(1729)

for (n in 1:1000) {
  if (num_ways(n) > 1) {
    print(paste(c(n, num_ways(n))))
  }
}

fib_cache <- c(1, 1, 2)
fib <- function(n) {
  if (is.null(fib_cache[n]) | is.na(fib_cache[n])) {
    
    fib_cache[n] <<- fib(n-1) + fib(n-2)
    return(fib_cache[n])
  } else {
    return(fib_cache[n])
  }
}

fib(20)

fib_test <- function(n, k) {
  values = c()
  for (i in 1:n) {
    values = c(values, fib(i))
  }
  elemwise = (values %% k) == 0
  return( c(1:n)[elemwise] )
}
# .269s unmemoized
tic()
fib_test(20,4)
toc()

#2.7740s unmemoized
# 0.02, 0.007 memoized
tic()
fib_test(25,4)
toc()

# 0.011
tic()
fib_test(40,6)
toc()


# exercise 1.6
set.seed(50)
xVec <- sample(0:999, 250, replace=TRUE)
yVec <- sample(0:999, 250, replace=TRUE)

a = yVec - xVec

# want vector length to be 249
newyVec = head(yVec, 249)
newxVec = tail(xVec, 249)
sin(newyVec) / cos(newxVec)

#c
results = c()
for (i in 1:(length(xVec)-2)) {
  results = c(results,
              xVec[i] + 2 * xVec[i + 1] - xVec[i + 2])
}

#d

# exp(-1 * xVec[i + 1])
# (xVec[i] + 10)
total = 0
for (i in 1:249) {
  total = total + (exp(-1 * xVec[i + 1]))/(xVec[i] + 10)
}
total

#1.7
#a
yVec[yVec > 600]
#b
c(1:(length(yVec)))[yVec>600]
#c
xVec[yVec>600]
#d
sqrt(abs(xVec - mean(xVec)))
mean_x = mean(xVec)
f <- function(x) {
  return(sqrt(abs(x - mean_x)))
}
sapply(xVec, f)

length(yVec[abs(max(yVec) - yVec) <= 200])
sum(abs(max(yVec) - yVec) <= 200)

#e
sum(xVec %% 2 == 0)

#g
order(yVec)
yVec[order(yVec)]
yVec[c(200, 24, 216)]
xVec[order(yVec)]

#h
yVec[seq.int(1, length(yVec), by = 3)]

#3.2
tmpFn <- function(x) {
  results = c()
  for (n in 1:(length(x) - 2)) {
    results = c(results, ((sum(x[n:(n+2)]))/3))
  }
  return(results)
}
tmpFn(c(1:5,6:1))

#3.9
#a
quadmap <- function(start, rho, niter) {
  values = c(start)
  for (k in 2:niter) {
    values[k] = rho * values[k-1] * (1 - values[k-1])
  }
  return(values)
}
quadmap(0.95, 2, 250)
tmp <- quadmap(0.95, 2.99, 500)
  # return vector(x1...xn) where xk = r * xk-1
    # (1 - xk-1)}
  
plot(tmp[100:500])

#b
