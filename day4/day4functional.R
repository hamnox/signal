df = data.frame(matrix(1:100, nrow=10))
df
means = sapply(1:ncol(df), function(i) { mean(df[[i]]) })
means

means = lapply(df, function(i) { mean(i) })
means

double = function(x) {
  2*x 
}
lapply(1:10, double)

# Why does lapply() return a list by default? 
# Lists are heterogeneous, so perhaps you want to return a result with
# different types

df = mtcars
unlist(lapply(1:ncol(df), function(c) class(df[[c]])))

df = mtcars
std_columns = function(df) {
  std_df = data.frame(lapply(1:ncol(df), function(c) { 
    (df[[c]] - mean(df[[c]])) / sd(df[[c]])
  }))
  colnames(std_df) = colnames(df)
  rownames(std_df) = rownames(df)
  return(std_df)
}
std_columns(df)

df = data.frame(matrix(1:100, nrow=10))
df[1:5] = lapply(df[1:5], as.character)
df
typeof(df[[1]])

std_num_columns = function(df) {
  std_df = data.frame(lapply(df, function(c) {
    if (is.numeric(c) == TRUE) {
      return( (c - mean(c)) / sd(c) )
    } else {
      return(c)
    }
    }))
  return(std_df) 
}
df = std_num_columns(df)
df
class(df[[4]])

my_lapply = function(args, func) {
  output = vector("list", length = length(args)) # preallocate space
  for (i in 1:(length(args))) {
    output[i] = func(args[i])
  }
  return(output)
}

args = rep(5, 1000)
output = vector("list", length = length(args))
length(output)
my_lapply(1:10, double)

matrix(rnorm(100), nrow=10)

sub_prev_col = function(df) {
  new_df = data.frame(df)
  for (i in 2:length(df)) {
    new_df[i] = df[i] - df[i-1]
  }
  return(new_df)
}

df = data.frame(matrix(1:100, nrow=10))
sub_prev_col(df)

L = lapply(1:5, function(x) sample(c(1:4, NA)))
L

meanFinder = function(l) {
  sapply(l, mean, na.rm = TRUE)  
}
meanFinder(L)
?vapply

addColNum = function(df) {
  for (i in 1:length(df)) {
    names(df)[i] = paste(names(df)[i],i,sep='_') 
  }
  return(df)
}
addColNum(df)



