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

# estimateSlopes Redo Week 2 with FUNCTIONAL PROGRAMMING!
# estimateSlopes = function(a, n, numTrials = 500) {
#   vapply(1:numTrials, function(a, n) {
#     df = getSamples(a, n)
#     return (coef(lm, y~ x, df)["x"])
#   }, 0, n=n)
# }
# NEVERMIND!

estimateSlopes(0.8, 1000, numTrials=500)

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
estimateSlopesWithPVals = function(a, n, numTrials = 500) {
  #results = list()
  slopes = c()
  pValues = c()
  for (i in 1:numTrials) {
    df = getSamples(a, n)
    fit = lm(y ~ x, df)
    slopes[i] = coef(fit)["x"]
    pValues[i] = coef(summary(fit))[2,4]
  }
  return(data.frame(slopes, pValues))
}

df = estimateSlopesWithPVals(0.1, 100)

coef(summary(fit))[2,4] # how to get the p-value

# p-values 2
df = estimateSlopesWithPVals(0.1, 500, numTrials = 10000)

# plot slopes and p-values
plot(df)

median(df$pValues)
length(df$slopes[df$slopes <= 0]) / length(df$slopes)

ggplot(df,aes(pValues, )) + geom_()

# Attributes, Factors, and Matrices

attributes(mtcars) #names, rownames, class
df = mtcars
attributes(df)$names = rep("huey", 11)
attributes(df)
attributes(df)$class = "list"
attributes(df)
class(df) # Whoa, this worked!

df = mtcars
attributes(mtcars)
print(df)
attributes(df)$row.names[1] = "Huey Row" 
print(df)
attributes(df)
attributes(df)$class = "list"
print(df) # get attr(,"class")
attributes(df)$class = "data.frame"
print(df) # fixes it
attributes(df)$class = "cucumber"
print(df)
# Conclusion: changing the class is bad

df = mtcars

?attr()
?attributes()

names(df)

doubleNames = function(df) {
  attr(df, "names") = sapply(attr(df, "names"), function(x) {
      return(paste(x,x,sep=''))
  })
}

# Factors
f = factor(c("A","B"))
levels(f)
f[3] = "C" # error
levels(f)
levels(f)[3] = "C"
levels(f)
f[3] = "C"
levels(f)

f2 = factor(c("Male","Female"))
f3 = c(f,f2) # No error, but no longer a factor, turns into a number vector
f3 
levels(f3)


# 2.2.2.3
f1 = factor(letters)
f2 = rev(factor(letters))
f3 = factor(letters, levels = rev(letters))

fruits = c("apple", "grapefruit", "NA", "apple", "apple", "-", "grapefruit", "durian")

fruitlevels = c("apple", "grapefruit", "durian")
refruited = factor(fruits, fruitlevels)
is.na(refruited[3])

arbcharacterNA = function(charvec) {
  dedupnoNA = unique(charvec)
  dedupnoNA = dedupnoNA[!is.na(dedupnoNA)]
  addNA(factor(charvec,dedupnoNA), ifany = TRUE)
}
arbcharacterNA(c("asdf", NA_character_, "sdfj", "ff", "ff"))
arbcharacterNA(c("asdf", "sdfj", "ff", "ff"))

round(.5 + -2:4)
( x1 <- seq(-2, 4, by = .5) )
x1[trunc(x1) != floor(x1)]
trunc(-1.5)
floor(-1.5)

df[,1]
factor(df[,1])
floor(ncol(df))

df = mtcars
convertFloorColsToFactors = function(df) {
  nCols = floor(ncol(df))
  df[,1:nCols] = lapply(df[,1:nCols], function(x) {
    factor(x)
  })
  return(df)
}
str(convertFloorColsToFactors(df))

df = mtcars
convertFiveUniqueToFactors = function(df) {
  for (colName in colnames(df)) {
    if (nrow(unique(df[colName])) <= 5) {
      elems = unlist(df[colName])
      df[colName] = factor(elems, unique(elems))
    }    
  }
  return(df)
}
str(convertFiveUniqueToFactors(df))

toyDF = data.frame(c(NA,1,2,2,3), c("A","B", NA,"D","D"))
names(toyDF) = c("X", "Y")
str(toyDF)
toyDF

replaceNAWithMode = function(df) {
  for (colName in colnames(df)) {
    if(is.factor(df[[colName]])) {
      topvalue = sort(table(df[colName]), decreasing = TRUE)[1]
      topname = names(topvalue)
      for(i in 1:(nrow(df[colName]))) {
        if(is.na(df[i, colName])) {
          df[i, colName] = topname
        }
      }
    }
    # what if the mode is NA? 
    # sort(table(df["mpg"]), decreasing = TRUE)
  
  }
  return(df)
}

str(replaceNAWithMode(toyDF))

set.seed(1)
col1 = c(NA,1,2,3,4,5,6,7,8,9,10)
col2 = c("D","B", NA,"D","D","D","D",NA,NA,"G","H")
toyDF = data.frame(col1, col2)
names(toyDF) = c("X", "Y")
str(toyDF)
toyDF

replaceNAWithRandom = function(df) {
  for (colName in colnames(df)) {
    if(is.factor(df[[colName]])) {
      nas = is.na(df[[colName]])
      distro = df[[colName]][!nas]
      # pull random value
      
      # nas is a vector of T/F, Trues are NAs
      # vector of random values, size of # of 
      numNAs = sum(nas)
      imputed = sample(distro, numNAs, replace = TRUE)
      
      df[[colName]][nas] = imputed
    }
  }
  return(df)
}
replaceNAWithRandom(toyDF)

df = mtcars[1:10,]
for (n in c("cyl", "am","carb")) {
  df[[n]] = factor(df[[n]])
}

# createIndicators = function(df) {
#   for (colName in colnames(df)) {
#     columnvalues = df[[colName]]
#     if(is.factor(columnvalues)) {
#         levelopts = levels(columnvalues[1])
#         for(lvl in levelopts[2:length(levelopts)]) {
#         #for(lvl in levelopts) {
#           newcolumn = as.integer(columnvalues == lvl)
#           df[paste(colName,"_",lvl, sep="")] = newcolumn
#         }
#     }
#   }
#   return( df)
# }

createIndicators = function(df) {
  factorcols = sapply(df, is.factor)
  lapply(df[factorcols], function(col) {
    as.list(levels(col))
    newcolumn = as.integer(unlist(col) == levels())
  })
}
str(createIndicators(df))

# load()
?load
load("time.dat")

str = "10:00P"
grep("([0-9][0-9]):([0-9][0-9][A-Z])", "10:00P")

gsub("\\((.*?) :: (0\\.[0-9]+)\\)","\\1 \\2", "(sometext :: 0.1231313213)")

str(df[["H2GH42"]])

df %>%
  mutate(H2GH42 = as.character(H2GH42),
         H2GH43 = as.character(H2GH43)) %>%
  filter((H2GH42 !="999996") & (H2GH42 !="999998")) %>%
  filter((H2GH43 !="999996") & (H2GH43 !="999998")) %>%
  mutate(H2GH42new = sapply(H2GH42,getTime), 
         H2GH43new = sapply(H2GH43,getTime)) -> fixeddf


ggplot(fixeddf) + geom_histogram(aes(x=H2GH42new), fill="blue", alpha = 0.2, binwidth = 2) + 
                  geom_histogram(aes(x=H2GH43new), fill="red", alpha = 0.2, binwidth = 2)

dim(df)
dim(filter(df, H2GH42 != "999996"))

# TODO: Account for sleeping past midnight
getTime = function(str) {
  hour = as.integer(substr(str,1,2))
  #print(str)
  if(substr(str,6,6) == "P") {
    hour = (hour + 12)
  }
  hour = hour + (as.integer(substr(str,4,5)) / 60)
  
  if(hour < 12) {
    return(hour+4)
  } else {
    return(hour-20) 
  }
  return(hour)
}

ggplot()

getTime("999998")
getTime("09:00A") # Expect
getTime("10:00P")
getTime("01:00A")
getTime("12:33P")
getTime(fixeddf[2453,2])

m = matrix(1:100, nrow=5, ncol = 20)
as.numeric(m)
dim(m) = c(20, 5)
m
as.numeric(m)

matrixTransform = function(m) {
  return(matrix(as.numeric(m), nrow = ncol(m), ncol = nrow(m)))
}
matrixTransform(m)