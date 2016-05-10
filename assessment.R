library('ggplot2')
install.packages('psych')
library('psych')
library('psych')
library('dplyr')

# Part1
# X = unif(0,1), Y = unif(0,x)
# calc expected value X given Y = y

# Simulate k trials for k = 1,000,000, plot qpot()
# pair of values where d of y depends on x
generateXY = function() {
  k = 1000000
  for (i in 1:k){
    x = runif(1)
    y = runif(1,0,x)
    if (i > 1) {
      results = rbind(results, c(x, y))
      } else {
      results = c(x, y)
      }
  }
  return(results)
}
results = generateXY()
qplot(results[,2], results[,1], xlab="y val", ylab="x val")


# expected value of X, approximate by binning and taking
# the mean of x in each bin... where bin width = w

w = 0.02
data.frame(x = results[,1], y = results[,2]) %>%
  mutate(bin = floor(y / w)) %>%
  group_by(bin) %>%
  summarize(miny = min(y),
            maxy = max(y),
            minx = min(x),
            maxx = max(x),
            evx = mean(x)) -> evs

View(evs)
qplot(evs$bin*w, evs$evx)
# not sure if this makes any sense...  How is it 0.75 for ~ 0.5?
# Oh, because this is based on Y-values and higher Xs are expected
# to also give low Y values.


# E(X|Y = y) = ((y - 1)/ln(y))!
# calculate these for lots of different Y
y = seq(0, 1, 0.01)
genx = factorial((y - 1) / log(y))
qplot(y, genx)
# these don't seem to match up at all... it bottoms at x=0.880ish?
# it goes up exponentially? it's really not alike at all.

# make a single dataframe with Monte Carlo and direct calc

data.frame(x = results[,1], y = results[,2]) %>%
  mutate(calcx = factorial((y - 1) / log(y))) -> ev2

ggplot(ev2, aes(x=y)) + geom_point(aes(y=x)) + geom_line(aes(y=calcx, color="red"))
# this makes no freaking sense. Can I get my money back on this stupid fairy advice?

data.frame(x = results[,1], y = results[,2]) %>%
  mutate(calcx = factorial((y - 1) / log(y)),
         bin = floor(y/w)) %>%
  group_by(bin) %>%
  summarise(avgy = mean(y), avgcalcx = mean(calcx), avgx = mean(x)) -> ev2
ggplot(ev2, aes(x=avgy)) + geom_point(aes(y=avgx)) + geom_line(aes(y=avgcalcx, color="red"))
# Yup. Still makes no freaking sense here.



# PART 2
df = psych::msq
# cleared variables to avoid weirdness
help(msq)
missingVals = colSums(is.na(df)) / nrow(df)

newdf = df[1]
for (i in 2:ncol(df)) {
  newdf = cbind(df[i], newdf)
  if (names(df[i]) == "scornful"){
    print("done")
    break
  }
}
newdf = cbind(newdf, Extraversion = df$Extraversion,
              Neuroticism = df$Neuroticism)

head(newdf)

# replace the nas with average
replace_nas = function(df) {
  for (i in 1:ncol(df)) {
    avgval = mean(df[!is.na(df[i]),i])
    # print(avgval)
    df[is.na(df[,i]),i] = avgval
  }
  return(df)
}

newdf = replace_nas(newdf)

# histograms for extraversino and neuroticism
( ggplot(newdf) + geom_histogram(aes(x=Extraversion)) )
( ggplot(newdf) + geom_histogram(aes(x=Neuroticism)) )
 # my teeth grind at the lack of time I have to finagle
 # these onto the same plot
( ggplot(newdf) + geom_density(aes(x=Neuroticism)) )
( ggplot(newdf) + geom_density(aes=(x=Extraversion)) )
# OH FOR THE LOVE OF---WHY WILL YOU NOT GO ONTO TH
# SAME PLOT WITHOUT REWRITING THE WHOLE DATA FRAME?!?!

( ggplot(newdf, aes(x = Extraversion, y=Neuroticism))
  + geom_jitter(width=1, height=1) + geom_smooth() )

View(newdf)

magicplot = function (origdf, whichcols, y=NA) {
  df = origdf[whichcols]
  resultnums = df[,1]
  resulttypes = rep(names(df[1]), nrow(df))
  for (i in 2:ncol(df)) {
    resulttypes = c(resulttypes, rep(names(df[i]), nrow(df)))
    resultnums = c(resultnums, df[,i])
    #str(resultnums)
  }
  # results$type = factor(results$type, names(df))
  if (!is.na(y)) {
    resultys = rep(origdf[[y]],length(resultnums))
    # str(resultys)
    return(data.frame(all = resultnums, types = resulttypes, y = resultys))
  }
  return (data.frame(all = resultnums, types = resulttypes))
}


temp = magicplot(sample_n(newdf, 200), c("wakeful", "tense"), y="Extraversion")

# doesn't look interesting on its own
ggplot(newdf, aes(x=wakeful, y=Extraversion)) + geom_jitter(width=1, height=1) + geom_smooth(formula=y~x, method=lm)
# doesn't look interesting on its own
ggplot(newdf, aes(x=tense, y=Extraversion)) + geom_jitter(width=1, height=1) + geom_smooth(formula=y~x, method=lm)
# doesn't look interesting on its own
ggplot(newdf, aes(x=upset, y=Extraversion)) + geom_jitter(width=1, height=1) + geom_smooth(formula=y~x, method=lm)
# of course, pretty indicative
ggplot(newdf, aes(x=sociable, y=Extraversion)) + geom_jitter(width=1, height=1) + geom_smooth(formula=y~x, method=lm)
# pretty negatively indicative
ggplot(newdf, aes(x=lonely, y=Extraversion)) + geom_jitter(width=1, height=1) + geom_smooth(formula=y~x, method=lm)
# eeh, maybe a little indicative
ggplot(newdf, aes(x=excited, y=Extraversion)) + geom_jitter(width=1, height=1) + geom_smooth(formula=y~x, method=lm)
# heavy distribution skew with some usefulness, maybe would work better as a binary variable
ggplot(newdf, aes(x=depressed, y=Extraversion)) + geom_jitter(width=1, height=1) + geom_smooth(formula=y~x, method=lm)
# eh, confident is kinda good
ggplot(newdf, aes(x=confident, y=Extraversion)) + geom_jitter(width=1, height=1) + geom_smooth(formula=y~x, method=lm)
# not interested
ggplot(newdf, aes(x=bored, y=Extraversion)) + geom_jitter(width=1, height=1) + geom_smooth(formula=y~x, method=lm)

ggplot(newdf, aes(color=Extraversion)) + geom_jitter(aes(x=bored, y=lonely), width=1, height=1)
# I have a theory that sociability-ish indicators cross referenced with happiness indicators are significant here

fitted_all = lm(Extraversion ~ . -Extraversion, data = newdf)
# adjusted R-squared of all the things is .1143, are you kidding me?
# Hmmm... I'm not sure how to check interactions quickly, figure out when to add thing1*thing2
# notable:
# wakeful negative
# tense negative
# upset positive
# sociable positive (no duh)
# lonely negative
# excited positive
# depressed negative
# confident positive
# bored positive

# what I really want to do is examine each of the factors
# individually for the assumption of normality 
# and then check individual correlations/coefficients
# but there's no time for that I'm out of time.