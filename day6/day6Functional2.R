m = matrix(1:9, nrow=3)
apply(m, 1, mean)
m
dim(m)
?dism
?apply
apply(m, 2, mean)
apply(m, c(1,2), mean)

outer(1:3, 1:4)

outer(1:3, 1:4, function(m,n) { return ((m %% n) == 0)})

min_matrix = function(n, m) {
  outer(1:n, 1:m, min)
}

min_matrix(5,5)

custom_min = function(a,b) min(c(a,b))
outer(1:5, 1:5, custom_min)

# Map
weight = function(values, weights) {
  mapply(weighted.mean, values, weights, MoreArgs=list(na.rm=TRUE))
}
# wtf, well it seems to give us NA if NA is in the weights
# if it's in the values it gets properly ignored

weight_orig = function(values, weights) {
  mapply(weighted.mean, values, weight)
}


set.seed(1)
values = lapply(1:10, function(x) rnorm(10))
weights = lapply(1:10, function(x) rnorm(10))
values[[1]][3] = NA
values
weights[[3]][7] = NA
weights
weight(values, weights)

weight_orig(values, weights)


new_vals = c(NA, values)
new_vals

my_sum = function(v) {
  Reduce("+", v)
}

my_sum(c(1:10))

my_union = function(l) {
  Reduce(union, l)
}

l = list(c(1,2),4:5, 6:30)
my_union(l)

my_intersect = function(l) {
  Reduce(intersect, l)
}

my_intersect(l)

left_equal_right = function(func,l) {
  left = Reduce(func, l)
  right = Reduce(func, l, right = TRUE)
  if (left == right) {
    return (left) 
  }
  return(NA)
}

nums = c(2,2,3,4)
left_equal_right("^",nums)

exp(2,3)
2^3

my_reduce = function(func, l) {
   temp = l[1]
   for (i in 2:length(l)) {
      temp = do.call(func, list(temp, i))
      #temp = func(temp, i)   
   }
   return(temp)
}

Reduce("+", 2)
my_reduce("+",nums)
?do.call

any = function(pred, l) {
  return (!is.null(Find(pred, l)))   
}

all = function(pred, l) {
  return (length(l) == length(Filter(pred, l)))
}

any(function(x) return(x==2), nums)
all(function(x) return(x==2), nums)
all(function(x) return(x==2), c(2,2))
Find(function(x) return(x==7), nums)

