# attributes(mtcars)
# attributes(mtcars)$names = c(1:11)
# attr(mtcars,"names") = paste(attr(mtcars,"names"),attr(mtcars,"names"), sep="")
# class(c(1:3))
# levels(factor(c(letters[1:26],letters[1:26])))

f1 = factor(letters)
f2 = rev(factor(letters))
f3 = factor(letters, levels = rev(letters))
f4 = factor(rev(letters))

f5 = factor(rownames(mtcars))
f5

fruits = c("apple","grapefruit", "NA", "apple", "apple", "-", "grapefruit", "durian")
factor(fruits, exclude=c("-","NA"))

arb = c("hjrWSK7Y;BQRAD;MYSHGF;KTYSIYKTXGHVB; FEASYTSIG;JU64RT543Q7ID;06DY7GGTEF;ADVREQUI6S5RTH;DFVEQ;NA")
arbi = unlist(strsplit(arb,";"))

arbit = as.character(arbi)
factor(arbit)


test_df = data.frame(first = c(1:5),second = c(3:7), third = c(5:9))
##why do you need to floor this
col_factor = function(df){
  li = list()
  for (i in 1:floor(ncol(df))){
    f = factor(unlist(df[i]))
    print (f)
    li[[i]] = f
    #li = list(li,f)
  }
  return (li)
}

##unique columns
factor_5 = function(df){
  li = list()
  df_logical = lapply(df, function(col) { length(unique(col)) <= 5})
  new_df = df[unlist(df_logical)]
  col_factor(new_df)
}


test2_df = data.frame(c(NA,NA,1,2,3))
## replace NA with most common
NA_common = function(df){
  fac_df = col_factor(df)
  for (i in 1:ncol(df)){
    fac = fac_df[[i]]
    # print ("fac")
    # print (fac)
    #replacement
    check = is.na(fac)
    names(check) = NULL
    if (any(check)){
      n_check = log_to_num(check)
      most = mode_find(fac)
      # print ("most")
      # print (most)
      rac = replace(fac,n_check,most) 
      names(rac) = NULL
      # print("rac")
      # print (rac)
      fac_df[[i]] = rac
      # print (fac_df)
    } 
  }
  return(fac_df)
}

log_to_num = function(log_vec){
  n_vec = c()
  for (i in 1:length(log_vec)){
    if (log_vec[i] == TRUE){
      n_vec = c(n_vec,i)
    }
    }
  return (n_vec)
}

mode_find = function(fact){
  fact = fact[!is.na(fact)]
  tab = tabulate(fact)
  m = max(tab)
  fact[match(m,tab)]
}

## replace NA with most common
NA_imputed = function(df){
  fac_df = col_factor(df)
  for (i in 1:ncol(df)){
    fac = fac_df[[i]]
    check = is.na(fac)
    names(check) = NULL
    if (any(check)){
      n_check = log_to_num(check) # indices
      current_column = fac
      for (j in n_check) {
        imputed = sample(fac, 1)
        rac = replace(current_column,j,imputed)  # change most
        names(rac) = NULL
        current_column = rac
        fac_df[[i]] = rac
      # print (fac_df)
      }
    } 
  }
  return(fac_df)
}

##Binary Dummy Variable
 binary_make_var = function(df){
     n = ncol(df)
     df_mod = df
     for (i in 1:n){
       print (i)
       print (df[i])
         if (is.factor(df[[i]])){
             l = levels(df[[i]])
             for (j in l[2:length(l)]){
                 new_col = factor()
                 new_col = factor(as.numeric(df[i] == j))
                 named = paste(names(df[i]),j, sep="_")
                 df_mod = cbind(df_mod, new_col)
                 print(df_mod)
                 names(df_mod)[ncol(df_mod)] = named
               }
           }
       }
     return(df_mod)
   }
 binary_make_var(fac_df)

fac_df = mtcars[1:10,]
for (n in c("cyl", "am", "carb")) { 
  fac_df[[n]] = factor(fac_df[[n]])
}

load("C:\\Users\\User\\Documents\\GitHub\\Signal-Data-Science\\time.dat")
str(df)


# During the school year, what time do you usually go to bed on week nights?
# ditto for summer

after_eight = function(t){
  time = as.character(t)
  print (t)
  if (nchar(time)!=6){return (NA)}
  a_p = substring(time,6,6)
  hour = as.numeric(substring(time,1,2))
  minute = as.numeric(substring(time,4,5))
  if (a_p == "P"){
    hour = hour + 12
  } else if (a_p == "A") {
      hour = hour + 24
  } else {
    return (NA)
  }
  if (hour==36){
    hour = 24
    }
  h = hour - 20
  m = minute/60
  final = h+m
  return(final)
}

time_after = function(df){
  df_new = data.frame()
  for (i in 1:ncol(df)){
    for (j in 1:nrow(df)){
      df_new[j,i] = after_eight(df[j,i])
    }
  }
  return (df_new)
}

time_df = time_after(df)

names(time_df)

(ggplot(time_df, aes())
  +geom_histogram(aes(V1),fill = "blue", alpha = 0.3)
  +geom_histogram(aes(V2),fill = "green", alpha = 0.3))

(ggplot(time_df, aes())
  +geom_density(aes(V1),fill = "blue", alpha = 0.3)
  +geom_density(aes(V2),fill = "green", alpha = 0.3))

# Matrices

matrix(1:100, nrow = 10)
m = matrix(1:100, nrow = 5, ncol = 20)
as.numeric(m)
transform = function(m) {
  matrix(m, nrow=ncol(m), ncol=nrow(m))
}
as.numeric(transform(m))

attributes(m)
attributes(m)$dim = c(20, 5)
attributes(m)
m

df = data.frame(matrix(1:100, nrow=10))
df[5,5] = NA
df[6,6] = NA

is.na(df) # outputs a logical matrix
df[is.na(df)] # applying the logical matrix should then you give a vector of two NAs

divisibleByK = function(df, k) {
  return(df[(df %% k) == 0]) 
}
divisibleByK(m, 20)

# Matrix multiplication
a = matrix(1:4, nrow=2)
b = matrix(2:5, nrow=2)
a
b
a * b  # It just multiples the components
b = matrix(2:5, nrow=1)
a * b # Error: non-conformable arrays

i = matrix(c(1,0,0,1), nrow=2)
a * i # Just multiplies the components

class(a[1])
class(a[1,])
a[1,]
min_matrix = function(n, m) {
  result = c()
  for (i in 1:n) {
    for (j in 1:m) {
      result = c(result, (min(i, j)))
    }
  }
  return(matrix(result, nrow=n, ncol=m))
}

t(min_matrix(3,3))

is_symmetric = function(m) {
  if (nrow(m) == ncol(m)) {
    return(all(m == t(m))) 
  } else {
    return(FALSE)
  }
}

is_symmetric(min_matrix(3,3)) #symmetric
is_symmetric(matrix(1:10, nrow=2)) #nonsquare
is_symmetric(matrix(1:9, nrow=3)) #square

?diag
trace = function(mat) {
  return(sum(diag(mat)))
}

trace(testm)
testm
trace(testm + testm)
testm2 = matrix(2:10, nrow=3)
trace(testm2)
trace(testm + testm2)
# does appear to be linear
trace(3*testm) # so that seemed to work

mystery = function(x) {
  matrix(c(cos(x), -sin(x), sin(x), cos(x)), nrow = 2)
}

mystery(0)
mystery(pi)
mystery(pi/2)
mystery(2*pi) # Because floating point imprecision

xs = seq(0, 10, 1)
plot(x=xs, y=cos(xs))

sapply(xs, mystery)

make2by2 = function(l) {
  dim(l) = c(2,2)
  return(l)
}

l = list(1,2,3,4)

l = list(list(1,2),2,3,4)
new = make2by2(l)

dim(l) = c(2,2)
class(l)
class(l) = "data.frame"
l
df = data.frame()
class(df)

# Speculation on use cases of list-matrices
# When would we use this cool list-matrix thing? 
# It kind of reminds me of a looser data frame.

testm
?mapply

matrix_mult = function(A, B) {
  r = nrow(A)
  c = ncol(B)
  result = matrix(nrow=r, ncol=c)
   for (i in 1:r) {
     for (j in 1:c) {
       result[i,j] = sum(A[i,] * B[,j])
     }
   }
  return(result)
}

rep(1,10000)
testm = matrix(rep(1,10000), nrow = 100)

tictoc::tic()
testm %*% testm #0.815s
tictoc::toc()
tictoc::tic()
matrix_mult(testm, testm) #1.19s
tictoc::toc()


v1 = matrix(c(1,2), nrow=2, ncol=1)
v2 = matrix(c(3,4), nrow=2, ncol=1)
l = list(v1,v2)

plot_column_vectors = function(l, x) {
  matrix = matrix(ncol = 2)
  for (v in l) {
    #matrix = rbind(matrix, t(v))
    matrix = rbind(matrix, t(mystery(x) %*% v))
  }
  # -1 because there are NA
  df = as.data.frame(matrix[-1,])
  print(df)
  colnames(df) = c("x", "y")
  return(ggplot(df) + geom_point(aes(x=x, y=y)))
}

x = seq(0, 2*pi, pi/8)
plot_column_vectors(l, 3)
plots = lapply(x, function(x) { plot_column_vectors(l, x) })
length(plots)
plots[[1]]
plots[[2]]
plots
crap = function (...) { grid.arrange(...,  ncol=4) }
crap(plots)

install.packages("gridExtra")
library("gridExtra")
grid.arrange(plots[[1]],plots[[2]])
grid.arrange(plots)
do.call(grid.arrange, c(plots, ncol=4))

# Construct several arbitrary square matrices of identical size
m1 = matrix(sample.int(9, 9), nrow = 3)
m2 = matrix(sample.int(9, 9), nrow = 3)
m3 = matrix(sample.int(9, 9), nrow = 3)
m4 = matrix(sample.int(9, 9), nrow = 3)

# matrix multiply then take the trace
m1 %*% m2
trace(m1 %*% m2)
trace(m2 %*% m1)
trace(m1 %*% m2 %*% m3) # 3071
trace(m1 %*% m3 %*% m2) # 3191
trace(m2 %*% m1 %*% m3) # 3191
trace(m2 %*% m3 %*% m1) # 3071
trace(m3 %*% m1 %*% m2) # 3071
trace(m3 %*% m2 %*% m1) # 3191

# There's a cycle, so if you go m1->m2->m3, m3->m1->m2, m2->m3->m1, it stays the same

trace(m1 %*% m3)
trace(m3 %*% m1)
m1
m3
m1 %*% m3
m3 %*% m1

# What if they are symmetric? Should stay the same
