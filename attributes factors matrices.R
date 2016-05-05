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

