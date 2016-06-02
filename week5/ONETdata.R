library("ggplot2")
library("tidyr")
library("dplyr")
# Exploration
# load skills data
skills = read.csv("datasets/db_20_3_text/Skills.txt", sep="\t", as.is=TRUE, na.strings=c("n/a", ""))

# load occupation data
occupationData = read.csv("datasets/db_20_3_text/Occupation Data.txt", sep="\t", na.strings=c("n/a",""))

# add occupation titles to skills
skills$title = occupationData$Title[match(skills$O.NET.SOC.Code, occupationData$O.NET.SOC.Code)]

# check occupation titles for NAs
sum(is.na(skills$title)) # None!


# check out what the invalid ones look like
# first make NAs in Not.Relevant blank so they stop screwing with stuff
skills[is.na(skills$Not.Relevant),"Not.Relevant"] = ""
View(skills[skills$Not.Relevant == "Y",])
# yup those are fine to drop.
skills = skills[skills$Not.Relevant != "Y",]

# check out the recommend.suppress ones
View(skills[skills$Recommend.Suppress == "Y",])
# remove the NAs
skills = skills[!is.na(skills$Standard.Error),]

# try to visualize the difference
( ggplot(skills, aes(x=Element.ID, y=Data.Value, color=Scale.ID))
+ geom_jitter() )

plotty <- skills %>% group_by(Element.ID) %>% dplyr::summarise(id = unique(Element.ID), sums = sum(Data.Value), means = mean(Data.Value))
( ggplot(plotty, aes(x=id, y=sums)) + geom_bar(stat="identity") )

sum(skills$Scale.ID == "LV") # 33285
sum(skills$Scale.ID == "IM") # 33285 exactly the same :-/

# load abilities data
abilities = read.csv("datasets/db_20_3_text/Abilities.txt", sep="\t", as.is=TRUE, na.strings=c("n/a", ""))

abilities$title = occupationData$Title[match(abilities$O.NET.SOC.Code, occupationData$O.NET.SOC.Code)]

# check out the invalid abilities
abilities[is.na(abilities$Not.Relevant),"Not.Relevant"] = ""
View(abilities[abilities$Not.Relevant == "Y",])
abilities = abilities[abilities$Not.Relevant != "Y",]

# check out the recommend.suppress ones
View(abilities[abilities$Recommend.Suppress == "Y",])
# remove the NAs
abilities = abilities[!is.na(abilities$Standard.Error),]
abilities = abilities[!abilities$Recommend.Suppress == "Y",]

sum(abilities$Scale.ID == "IM") # 49451
sum(abilities$Scale.ID == "LV") # 39832

silenced = filter(abilities, Scale.ID == "IM") %>% group_by(Element.Name) %>% dplyr::summarise(jobs = mean(Data.Value) * length(unique(O.NET.SOC.Code)))


# load knowledge
knowledge = read.csv("datasets/db_20_3_text/knowledge.txt", sep="\t", as.is=TRUE, na.strings=c("n/a", ""))

knowledge$title = occupationData$Title[match(knowledge$O.NET.SOC.Code, occupationData$O.NET.SOC.Code)]

# check out the invalid knowledge
knowledge[is.na(knowledge$Not.Relevant),"Not.Relevant"] = ""
View(knowledge[knowledge$Not.Relevant == "Y",])
knowledge = knowledge[knowledge$Not.Relevant != "Y",]

# check out the recommend.suppress ones
View(knowledge[knowledge$Recommend.Suppress == "Y",])
# remove the NAs
knowledge = knowledge[!is.na(knowledge$Standard.Error),]
knowledge = knowledge[!knowledge$Recommend.Suppress == "Y",]

sum(knowledge$Scale.ID == "IM") # 49451
sum(knowledge$Scale.ID == "LV") # 39832

silenced = filter(knowledge, Scale.ID == "IM") %>% group_by(Element.Name) %>% dplyr::summarise(jobs = mean(Data.Value) * length(unique(O.NET.SOC.Code)))



# load occupation data
occupationData = read.csv("datasets/db_20_3_text/Occupation Data.txt", sep="\t", na.strings=c("n/a",""), as.is=TRUE)

# load skills data
skills = read.csv("datasets/db_20_3_text/Skills.txt", sep="\t", as.is=TRUE, na.strings=c("n/a", ""))

# load knowledge
knowledge = read.csv("datasets/db_20_3_text/knowledge.txt", sep="\t", as.is=TRUE, na.strings=c("n/a", ""))

# load abilities data
abilities = read.csv("datasets/db_20_3_text/Abilities.txt", sep="\t", as.is=TRUE, na.strings=c("n/a", ""))


# split datasets along 
imskill_m = skills %>% filter(Scale.ID == "IM") %>%
  select(O.NET.SOC.Code, Element.Name, Data.Value) %>%
  mutate(Element.Name = paste0("s-",Element.Name))

imability_m = abilities %>% filter(Scale.ID == "IM") %>% 
  select(O.NET.SOC.Code, Element.Name, Data.Value) %>%
  mutate(Element.Name = paste0("a-",Element.Name))

imknowledge_m = knowledge %>% filter(Scale.ID == "IM") %>% 
  select(O.NET.SOC.Code, Element.Name, Data.Value) %>%
  mutate(Element.Name = paste0("k-",Element.Name))

total = rbind(imskill_m, imability_m, imknowledge_m)
total = tidyr::spread(total, Element.Name, Data.Value)

# check for errors
sapply(total, function(x) sum(is.na(x)))

# get a tfidf (which may or may not really work at all)
library("textir")
tfidf = textir::tfidf(as.matrix(round(total[-1] - 1)))

load("datasets/tsneresult.Rdata")
# tsneresult = tsne::tsne(dist(total[-1] - 1), k = 3, perplexity=30)
save(tsneresult, file = "datasets/tseneresult.RData")
ggplot(as.data.frame(tsneresult), aes(x=V1, y=V2, color=V3)) + geom_jitter()

# split into train and validation data
set.seed(1992)
validate_indices = sample(seq(total), round(nrow(total) * 0.10))
train_set = total[-validate_indices,]
validate_set = total[validate_indices,]

# install.packages("plot3D")
library("plot3D")
scatter3D(tsneresult[,1], tsneresult[,2], tsneresult[,3])

# what clusters do I find these?
library("caret")


# get hierarchical clusters
clust = hclust(dist(train_set[-1] - 1), method="ward.D2")
plot(clust)

# Convenience function
print_clusters = function(df, labels, k) {
  for(i in 1:k) {
    print(paste("cluster", i))
    lookup = df[labels == i, 1]
    print(occupationData$Title[match(lookup, occupationData$O.NET.SOC.Code)])
    #print(df[labels == i, 1])
    #relrows = df[labels == i,]
    #print(colMeans(relrows)[colMeans(relrows) > 2])
  }
}

# Convenience function
library("cluster")
library("pvclust")
hclust_plot = function(d, method,k){
  scaled = scale(d)
  distance = dist(scaled)
  uncut = hclust(distance, method=method)
  cut = cutree(uncut, k=k)
  return(
    clusplot(scaled, cut, color=TRUE, shade=TRUE, labels=0, lines=0)
  )
}

cut_clust = cutree(clust, k = 5)
options(digits = 3)
print_clusters(train_set, cut_clust, 10)

# this plot proves unhelpful at any k
hclust_plot(train_set[-1], "ward.D2", 20)




plot(clust)
load("datasets/pvclust.Rdata")
# pval_clust = pvclust(t(train_set[-1]), method.hclust="ward.D2", method.dist="euclidean", parrallel=TRUE)
save(pval_clust, file = "datasets/pvclustresult.RData")
pvrect(pval_clust, alpha = 0.96)
picks = pvpick(pval_clust, alpha=0.96)
for (cluster in picks$clusters) {
  print(occupationData$Title[match(train_set[cluster,1], occupationData$O.NET.SOC.Code)])
}

library("fpc")
# kpicks = kmeansruns(scale(train_set[-1]), 8:100, criterion="ch", critout=TRUE)
# best k by ch = 8
# kpicks2 = kmeansruns(scale(train_set[-1]), 12:100, criterion="asw", critout=TRUE)
# best k by asw = 13

kpicks = kmeansruns(train_set[-1], 1:30, criterion="ch", critout=TRUE)
kpicks2 = kmeansruns(train_set[-1], 1:30, criterion="asw", critout=TRUE)
ggplot(data.frame(), aes(x=1:30)) + geom_point(aes(y=scale(kpicks$crit)), color="red", alpha=0.3) + geom_point(aes(y=scale(kpicks2$crit)), alpha=0.3)


# look at jobs most heavily loaded on each skill


# Did they categorize similar jobs *well*? In terms of skills/abilities/knowledge?