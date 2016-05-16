library("ggplot2")
library("dplyr")
library("glmnet")
# install.packages("pROC")
library("pROC")
# install.packages("corrplot")
library("corrplot")

speeddating_df = read.csv("day9/speeddating_aggregated_data.csv")
names(speeddating_df)

# sum(sapply(speeddating_df, is.na))
# documentation: https://github.com/signaldatascience/R-curriculum/blob/master/pdfs/week2/day4/speeddating-documentation.txt
# okay... so predicting gender using activities and carreers
View(speeddating_df["career_c"])
####AAAAAGGGH WHY DID SOMEONE FILL THESE IN AS MEANS THEY'RE FACTORS @#$*@#&($)
# sapply(speeddating_df, function(x) {)}) problem in race as well, but
# whatever

col = speeddating_df$career_c
replace_idiocy = function(col) {
  replace_needed = col == 5.27779126213592
  col[replace_needed] = sample(col[!replace_needed], sum(replace_needed), replace=TRUE)
  return (col)
}


# --------------------------------------------
# SETUPS -------------------------------------
# --------------------------------------------
read.csv("day9/speeddating_aggregated_data.csv") -> speeddating_df
  # mutate(career_c = replace_idiocy(career_c)) -> speeddating_df

speeddating_df %>% select(-(X:career_c), gender) -> activities_df

# 2 = Academic/Research
# 7 = Banking/Consulting/Finance/Marketing/Business/CEO/Entrepreneur/Admin
speeddating_df %>% select(-X, -race, -race_o, career_c) %>%
  filter(career_c %in% c(2, 7)) %>%
  mutate(is_academia = as.numeric(career_c == 2)) %>%
  select(-career_c) -> career_df

# caucasion = 2
# asian = 4
speeddating_df %>% select(-X, race, -race_o, -career_c) %>%
  filter(race %in% c(2, 4)) %>%
  mutate(is_asian = as.numeric(race == 4)) %>%
  select(-race) -> race_df

to_probability = function(logodds) {
  return(exp(logodds))
}

# --------------------------------------------
# --------------------------------------------
# --------------------------------------------

model_activities = glm(gender ~ .,
                       data = activities_df,
                       family = "binomial")
# wish I had time to finagle around glmnet

coefficients = coef(model_activities)[seq(coef(model_activities))]
coefficients[order(coefficients)]
# shopping     theater      hiking    exercise          tv    clubbing 
# -0.27009048 -0.18632249 -0.17769131 -0.10875150 -0.10856425 -0.09203367 
# yoga     reading         art    concerts      dining       music 
# -0.07593917 -0.05774619 -0.04508177 -0.02852368 -0.01131976  0.01655134 
# tvsports     museums      movies      sports      gaming (Intercept) 
# 0.02062807  0.06440403  0.12651321  0.22985598  0.29563642  2.67270581

# interpeting:
# see to_probability fn written in setup
# intercept
to_probability(-0.27009048) # 0.7633104
# probably the base rate if my data was normalized before inputting
# each interest point in gaming, sports, and movies increases the
  # log odds a fair clip...
# shopping, theater, hiking decrease the log odds
corrplot(as.matrix(coefficients[-1], ncol=1), is.corr=FALSE)

# looks decent here
roc(activities_df$gender, predict(model_activities), plot=TRUE)
# .8252

# looks skewed but kinda terrible here
ggplot(cbind(activities_df, yhat = model_activities$fitted.values),
       aes(x=gender, y=yhat, color=gender)) + geom_jitter()


#----------------------------------------
# predict whether in academia or business/finance
model_career = glm(is_academia ~ .,
                       data = career_df,
                       family = "binomial")

career_coefficients = coef(model_career)
career_coefficients
# WRONG
# (Intercept)        gender        attr_o       intel_o        sinc_o 
# -0.4370875820 -1.1305835613 -0.5963146452  3.2890116257 -0.1582065219 
# fun_o         amb_o         dec_o        race_o          race 
# 0.0534398069 -2.9340679773  0.1888943206  0.5914693899 -0.1345932903 
# sports      tvsports      exercise        dining       museums 
# -0.0839590962 -0.0369734897 -0.0006267079 -0.1110102452  0.1267736252 
# art        hiking        gaming      clubbing       reading 
# 0.0943522350  0.1035295227 -0.1533486399 -0.1482001169  0.1109702090 
# tv       theater        movies      concerts         music 
# -0.0969595680 -0.0520582938 -0.0087548136  0.0642948547  0.0175709517 
# shopping          yoga 
# 0.1098039467 -0.0639512977

# intercept is -0.437... so given no interests and no good qualities
# you may be in business lol
to_probability(-0.437) # 0.6459714
corrplot(as.matrix(career_coefficients[-1], ncol=1), is.corr=FALSE)
# intelligence points add 3.289 to your log odds
# the other major determinant is ambition, unsurprisingly that
# increases the likelihood you're in business
# gender(male) is another determinant and --wait, I have race in here
# as a number.



# (Intercept)       gender       attr_o      intel_o       sinc_o 
# 1.785397766 -1.067938364 -0.594695300  3.141829949 -0.147925764 
# fun_o        amb_o        dec_o       sports     tvsports 
# 0.090754020 -2.955692019  0.404880630 -0.096260056 -0.039093064 
# exercise       dining      museums          art       hiking 
# 0.011489382 -0.117840236  0.146796532  0.070746479  0.092489352 
# gaming     clubbing      reading           tv      theater 
# -0.162377591 -0.124831590  0.092790788 -0.099691790 -0.063439564 
# movies     concerts        music     shopping         yoga 
# -0.008606696  0.078021115  0.029214184  0.100336528 -0.061401956
corrplot(as.matrix(career_coefficients[-1], ncol=1), is.corr=FALSE)
# it looks visually the same anyways, but the intercept is different.
to_probability(career_coefficients[1])
# 64% baseline if I have it right, which pushes slightly towards
# academia. let's see:
mean(career_df$is_academia) # 0.5135135

# looks decent here
roc(career_df$is_academia, predict(model_career), plot=TRUE)
# .8928

# visually, that's pretty good
ggplot(cbind(career_df, yhat = model_career$fitted.values),
       aes(x=is_academia, y=yhat, color=intel_o^3)) + geom_jitter()

#--------------------------
# now doing race!

model_race = glm(is_asian ~ .,
                   data = race_df,
                   family = "binomial")

race_coefficients = coef(model_race)
race_coefficients
# (Intercept)        gender        attr_o       intel_o        sinc_o 
# -5.2393535100 -0.6026090530 -0.5143715664  0.4671641603  0.4272951825 
# fun_o         amb_o         dec_o        sports      tvsports 
# -0.3486491508  0.3688603428 -0.5970257312 -0.0065063083  0.1086015674 
# exercise        dining       museums           art        hiking 
# -0.0923607030  0.1259067060 -0.2418075783  0.0723399799 -0.0002000897 
# gaming      clubbing       reading            tv       theater 
# 0.0442847873  0.0223269566  0.0006527237  0.0752498380  0.0316662679 
# movies      concerts         music      shopping          yoga 
# -0.1122603646  0.0355052839 -0.0129488242  0.2389697450 -0.0065847219

race_coefficients[order(race_coefficients)]

# interesting that the intercept is so low. There's clearly a lot of
# other stuff going on to turn it up. 
# exp(-5.239) ... I think I've been doing probability wrong
  # edit: fixed now

corrplot(as.matrix(race_coefficients[-1], ncol=1), is.corr=FALSE)
# wait, GENDER is an anti-predictor? one of the biggest.
# as is attractiveness... ouch... and getting decided against.
# asians mildly less fun on average
# meanwhile, intelligenc, sincerity and ambition are positive.
# ... also shopping a little?
# in general, I think I need to check out some interference
# between variables
to_probability(race_coefficients[1]) #0.005303685 
mean(race_df$is_asian) # 0.3090909

# looks decent here
roc(race_df$is_asian, predict(model_race), plot=TRUE)
# .7824 not as good

ggplot(cbind(race_df, yhat = model_race$fitted.values),
       aes(x=is_asian, y=yhat, color=intel_o^3)) + geom_jitter()
# what I really ought to do here is plot residuals somehow...
# just bin em.