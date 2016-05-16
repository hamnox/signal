

# NAMES
# --------------------------------
# AAAGGGH WHY R WHY
c(one=1, two=1, one=2)["one"]
  # can multiple thing

c(1, 2, 3)["random"]
  # gives you NA

c("one"=1) # yup
c(date()=1) # nope
c([[1]]=1) # nope
c(TRUE=1)
c(T=1) # coerce to string

veccy = c(1, 2, 3, 4, 5)
names(veccy) = c(date(), date(), date(), date(),
                 date())
typeof(names(veccy))
names(veccy) = c(TRUE, FALSE, TRUE, FALSE, TRUE)
typeof(names(veccy)) # character coercion
veccy[FALSE]
veccy[c(F, T, T)]
veccy[date()]


# with a longer name vector, it just says no
# actually it says it must be the same length
# WHICH IS A LIE
names(veccy) = c("a", "b", "c", "d", "e", "f", "g", "h")
veccy

x = c(a=1, b=2, c=3)
x["a"]
x[c("c", "b")]

# when more than one element has the same name and you
# try to access it, it simply gives you the first occurrence

typeof(list()) # a list..
unlist(list("a", 1, 2))

unlist(list("a", 1, 2, list("b", TRUE)))
# It flattens the list and turns into a vector?!?!?!

c(one=1, two=1, one=2)$one # not for atomic vectors
x = c(a=1,b=2,c=3)
x["a"]
example = list("a", 1, 2, list("b", TRUE))
names(example) = c("a","b","c","d")
example$a # this returns a value
example$d
example["a"] # this returns another list
example[a]

c(c(1,2,3), list("a", TRUE, FALSE))
# you get a list. huh?
# it probably increases the generality to make it work

example = c()
example[1] = list("a", TRUE)
example[2] = list("f", 1, 3)
example
# can't do it cause you try to put two items in 1 index spot
# IT SHOULD WORK
# ok, vectors aren't parametrically polymorphic
# constrained on atomics or something

list(c(1:5), c("a", "B"))
# so that's fine

# Data Frames
# --------------------------------

df = data.frame(matrix(1:100, nrow=10, ncol=10))
typeof(df)
class(df)

as.data.frame(veccy)
length(as.data.frame(veccy)) # == 1

example = list("a", "b", c(3, 4))
as.data.frame(example) # flattens extra list, duplicates vector

example = list(list("a", "b"), list("c", "d"), list("e", "f", "g"))
example = list("a", "b", c(3, 4), c("ha", "ho", "hum", "fo"))


dim(cbind(df, df))
dim(rbind(df, df, df, df))

dim(do.call(cbind, rep(df,10)))

# what is factor what what even
as.vector(example)
is.vector(example)
is.atomic(example)

data.frame()


# Subsetting in R
# --------------------------------
str(df)

x = 1:5
x[c(3, 1)] # == 3 and 1
x[-c(3, 1)] # == 2, 4, 5 (exclude)
x[c(-3, -1)] # == 2, 4, 5
x[c(1,0,1,0,1)] # == 1 1 1
x[c(3,3,3,3,4,4)]
x[c(1.2, 2.99999999)] # truncates
# mixed positive negative gets a :(

x[c(TRUE, FALSE)]
x[FALSE]

x[c(1, NA, 5)] # NA just gives you NA back
x[c("namey")] # return NA

y = list("EVIL", "CON", "CARNE", subl=list(7, 8, 9), 10)
y[TRUE]
y["subl"] # get a list back of all the things
unlist(y[2])

df["5",c("X1", "X3")] # column if one vector
# if you add a comma it becomes row, column
df["5",] 
typeof(df[,1])
class(df[1])

# Supplementary exercises
# --------------------------------
x = 1:5
x[c(1,2)] = c(10,11)
x

x = 1:10
x[c(FALSE, TRUE)] = c(100, 100, 10, 100, 100)
x

x = 1:5
x[NA] # thinks its a logical
x[c(FALSE, 9, TRUE)]

str(mtcars)
mtcars[1:20] # not enough columns
mtcars[1:20,] # 20 rows

df[3,4] = NA
df[is.na(df)] = 0 # tests for NAs, takes out, assigns 0 to
df


x = c("a", "b", "a", "a", "b", "x", "b", "a")
fruits = c(a="apple", b="banana", x=NA)
fruits[x]

colnames(df) = c("b", "q", "d", "z", "f", "a", "c", "e", "g", "h")

order_columns = function(df) {
  colname_order = order(colnames(df))
  return(df[colname_order])
}

order_columns(df)

# sample
afunction = function(df, rows=FALSE) {
  col_order = sample(1:(length(df)))
  if (rows) {
    row_order = sample(1:length(df[[1]]))
    return(df[ row_order, col_order ])
  }
  return(df[col_order]) # random permutated columns
}

afunction(df, TRUE)

afunction = function(df, k) {
  col_order = sample(1:(length(df)), size=k, replace=TRUE)
  return(df[col_order]) # random permutated columns
}

afunction(df, 1)

afunction = function(df, m) {
  i = sample(1:(length(df[[1]]) - m + 1), size=1)
  return(df[i:(i+m-1),])
}

afunction(df, 3)

# colname is a string, so need to wrap in vector length 1
afunction = function(df, colname) {
  return(df[colnames(df) != colname])
}

# Miscellaneous Problems in R
# --------------------------------
# lists

nesting_depth = function (L) {
#  print("206")
  if (is.atomic(L)) {
  #  print("208")
    return(0)
  }
  top_yet = 1
  if(length(L) != length(c(L))) {
    for (sublist in L) {
      print(paste("wtf: ", sublist))
      sublevels = nesting_depth(sublist)
      if ((sublevels + 1) > top_yet) {
        top_yet = sublevels + 1
      }
    }
  }
  return(top_yet)
}

nesting_depth = function(L) {
  if (is.atomic(L)) {
    return(0)
  } else {
    return(1 + max(unlist(lapply(L, nesting_depth))))
  }
}

one = list(1)
nesting_depth(one)
list(one, one)
nesting_depth((list(list(one, one))))


ndomino = function(n) {
  result = lapply((1:n), function(x) { lapply((x:n), function(y) { return(list(x, y)) }) })
  return(result)
}

View(ndomino(3))

circle_dominoes = function(dominoes) {
  len = length(dominoes)
  if(len < 2) {
    return(FALSE)
  }
  if (dominoes[[1]][[1]] != dominoes[[len]][[2]]) {
    return(FALSE)
  }
  for (i in 2:len) {
    if (dominoes[[i]][[1]] != dominoes[[i-1]][[2]]) {
      return(FALSE)
    }
  }
  return(TRUE)
}

domino = list(list(1,3), list(3, 6), list(6,1))
str(domino)
domino[[1]][[1]]
circle_dominoes(domino)
domino2 = list(list(1,3), list(3, 5), list(6,1))
circle_dominoes(domino2)

grepblah = function(df) {
  ivec = rep(0,26)
  names(ivec) = letters
  chars = unlist(lapply(colnames(df), function(x) {strsplit(x, split="")}))
    for (char in chars) {
      ivec[char] = ivec[char] + 1
    }
  return(ivec)
}
grepblah(df)

# gsub
gsuuub <- function (df) {
  cols = colnames(df)
  for(i in 1:(length(cols))) {
    cols[i] <- sapply(cols[i], function(string) {
      return(paste(gsub(" ", ".", string), "_mod",sep="")) })
  }
  return(cols)
}
gsuuub(df)
df

# remove last 4, didn't do

row_names = function(df) {
  sep = c("_")
  names(sep) = c("sep")
  do.call(paste, c(as.list(rownames(df)), sep))
}
row_names(df)

# spiral -- are you kidding me

# divisibility properties
longestrun = function(invective) {
  # longest last run was = 1
  # if current run, current length of that
  
  longest_last = 1
  longest_char = invective[1]
  last_char = invective[1]
  cnt = 1
  for( i in 2:length(invective) ) {
    if( invective[i] == last_char ){
      cnt = cnt + 1
      if(cnt > longest_last) {
        longest_last = cnt
        longest_char = invective[i]
      }
    } else {
      cnt = 1
      last_char = invective[i]
    }
  }
  return(rep(longest_char, longest_last))
}
longestrun(c(1, 2, 2, 2, rep(3,10)))
# as.data.frame(rle(c(1, 2, 2, 2, rep(3,10))))

memoized <- c(2, 3, 5)
pm = function(primemaybe) {
  root = round(sqrt(primemaybe))
  maxmemo = max(memoized)
  if (root <= maxmemo) {
    for (factor in memoized) {
      if (primemaybe %% factor == 0){
        return(FALSE)
      }
      if (factor > root) {
        break
      }
    }
    return(TRUE)
  }
  for (factor in (maxmemo + 1):root) {
    prime_fac = pm(factor)
    if (prime_fac) {
      memoized <<- c(memoized, factor)
    }
    if (primemaybe %% factor == 0){
      return(FALSE)
    }
  }
  return(TRUE)
}
pm(43)
# max * min, next min, next min

