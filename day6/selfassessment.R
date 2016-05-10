# 10:05am
library("ggplot2")


# Suppose that X is uniformly distributed over [0,1].
# Now choose X = x and let Y be uniformly distributed over [0,x].
# Is it possible for us to calculate
# "the expected values of X given Y = y", ie E(X|Y = y)?

# simulate k trials, where k = 1000000
k = 1000000

xs = rep.int(1, k)
xs = runif(xs)
ys = vapply(xs, function(x) {runif(1, max=x)}, 0.1)

df = data.frame(x=xs, y=ys)

qplot(x, y, data=df[1:500,])

# separate values of Y into equal width bins
# takes the mean of X within each bin
binning = function(df, w) {
  bins = seq(0, 1-w, w)
  means = vector(length = (length(bins)))

  i = 1
  for (bin_min in bins) {
    selector = bin_min < df$y & df$y < bin_min+w
    mean_x = mean(df$x[selector])
    means[i] = mean_x
    i = i+1
  }
  results = data.frame(bin_start=bins, mean_x = means)

  return(results)
}

binned_results = binning(df, 0.01)
qplot(bin_start, mean_x, data=binned_results)
# Yes, makes sense -- if y was higher, x was likely higher as well

theoretical_x = vapply(df$y, function(y) { (y - 1) / log(y) }, 0.1)

qplot(df$y[1:1000], theoretical_x[1:1000]) # looks quite similar

df = cbind(df, theoretical_x)
str(df)
p = ggplot(df[500:1500,])
p + geom_point(aes(x=x, y=y)) + geom_smooth(aes(x=x, y=theoretical_x))

# Part 2!

library("psych")
help(msq)
df = msq
str(df)

# compute fraction of missing values, and sort in descending order
features = colnames(df)
selector = lapply(df, function(col) { sum(is.na(col)) / length(col) })
ordered_features = features[order(unlist(selector), decreasing=TRUE)]
# I guess what was really wanted was
ordered_fractions = selector[order(unlist(selector), decreasing=TRUE)]

# make new dataframe
new_df = cbind(df[1:75], Extraversion=df$Extraversion, Neuroticism=df$Neuroticism)
str(new_df)

name_to_col = function(name) {
  print(str(new_df))
  sapply(new_df$name, function(v) {
    if (is.na(v)) return(ordered_fractions$name) else return(v)
  })
}

impute = function(v) {
  if (is.na(v)) return(ordered_features$)
}

new_df2 = lapply(names(new_df), )
})

for (name in)

str(new_df2)
