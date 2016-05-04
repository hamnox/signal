plot(x,dnorm(x,0,1.5),type="l",col="blue")

x = (-10):10
#qnorm(head(x, n=10), 
      
      
plot(seq(0,1,0.01),qnorm(seq(0,1,0.01),0,50),type="l")


qnorm(.9,400,83)

qnorm(.25,0,1)
qnorm(.25)

# mean = 500
# sd = 100

pnorm(600, mean = 500, sd = 100) # Alf is 84 percentile

a = seq(0.1,0.9,1)
a

n = c(100, 500, 2500, 10000)

# Regressions with simulated data
getSamples = function(a, n) {
  x = rnorm(n)
  b = sqrt(1 - a^2)
  error = rnorm(n, sd = b)
  y = a*x + error
  return(data.frame(x, y))
}

df = getSamples(0.1,1000)
ggplot(df, aes(x, y)) + geom_point() + geom_smooth(method = "lm")

df = getSamples(0.9,1000)
ggplot(df, aes(x, y)) + geom_point() + geom_smooth(method = "lm")

estimateSlopes = function(a, n, numTrials = 500) {
  results = c()
  for (i in 1:numTrials) {
    df = getSamples(a, n)
    results[i] = coef(lm(y ~ x, df))["x"]
  }
  return(results)
}

# As n increases, we start getting a more normal distribution
someestimates = data.frame(x=estimateSlopes(0.8,1000,numTrials=3000))
ggplot(someestimates, aes(x)) + geom_histogram(binwidth=.005)

slopes = seq(0.1,0.9,0.1)
sample_sizes = c(100, 500, 2500, 10000)

sd(estimateSlopes(0.1, 100))

dfSD = data.frame()

std_devs = c()
for (slope in slopes) {
  for (num in sample_sizes) {
    slope_sd = sd(estimateSlopes(slope, num))
    #print(slope_sd)
    std_devs = c(std_devs, slope_sd)
  }
}

my_matrix = matrix(std_devs, nrow = length(sample_sizes))

dfSD = data.frame(my_matrix)
dfSD
rownames(dfSD) = sample_sizes
colnames(dfSD) = slopes # annoyed can't graph

n = seq(100,10000,200)
std_devs = c()
for (num in n) {
  slope_sd = sd(estimateSlopes(0.1, num))
  #print(slope_sd)
  std_devs = c(std_devs, slope_sd)
}

dfSD2 = data.frame(std_devs)
rownames(dfSD2) = n
dfSD2
plot(n, std_devs, type="p")

# p-values
