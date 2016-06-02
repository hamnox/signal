library("readr")
library("cluster")
library("pvclust")
library("fpc")
library("ggplot2")
options(digits=3)

setwd("C:/Users/User/Documents/GitHub/Signal-Data-Science")
protein_df = read.delim("~/GitHub/Signal-Data-Science/protein.txt")

rownames(protein_df) = protein_df$Country
protein_df$Country=NULL


# Scale the data
protein_matrix = scale(protein_df)
protein_matrix

# Generate distance matrix
dist_matrix = dist(protein_matrix, method="euclidean")
dist_matrix

# Create a cluster
clust = hclust(dist_matrix, method="ward.D2")
plot(clust)

# Convenience function

print_clusters = function(labels, k) {
  for(i in 1:k) {
    print(paste("cluster", i))
    print(protein_df[labels == i, c( "RedMeat",
                                 "Fish", "Fr.Veg")])
  }
}

# Cut trees into 5
cut_clust = cutree(clust,k=5)
print_clusters(cut_clust, k=5)

# Convenience function

hclust_plot = function(d, method,k){
  scaled = scale(d)
  distance = dist(scaled)
  uncut = hclust(distance, method=method)
  cut = cutree(uncut, k=k)
  return(
    clusplot(scaled, cut, color=TRUE, shade=TRUE, labels=2, lines=0)
  )
}

# Plot the first two principle components
for (k in 2:5) {
  hclust_plot(dist_matrix, "ward.D2", k)
}

# bootstrap sampled clusters (new with P-values!)
# transpose first, pvclust goes by columns instead of rows
pval_clust = pvclust(t(protein_matrix), method.hclust="ward.D2", method.dist="euclidean")


# K means clustering

k_mean = kmeans(protein_matrix, 5)

kmeans_plot = function(data,k){
  k_mean = kmeans(data,k)
  return(clusplot(data, k_mean$cluster, color=TRUE, shade=TRUE, labels=2, lines=0))
}
# fairly not stable with k=5
# extremely stable with k=2

artificial = rbind(China = rnorm(ncol(protein_matrix), sd=2), protein_matrix)

# the Chinese ruin everything

# Validating a choice of K

# NEVER EVER put plot=TRUE
k_meanrun = kmeansruns(protein_matrix, krange=1:10, criterion="ch")
k_meanrun2 = kmeansruns(protein_matrix, krange=1:10, criterion="asw")

k_mean_compare = data.frame(k=1:10, asw=scale(k_meanrun2$crit), ch=scale(k_meanrun$crit))
ggplot(k_mean_compare, aes(x=k)) + geom_point(aes(y=asw), color="darkgreen", size=5, alpha=0.5) + geom_point(aes(y=ch), color="purple", size=4, alpha=0.7)

# The criteria disagree: aws supports 3 (slight), ch supports 2(lots)

# run bootstrap k-means
k_bootstrap = clusterboot(protein_matrix, clustermethod=kmeansCBI, runs=100, iter.max=100, krange=5)

# get clusters on original data
bootstrap_clust = k_bootstrap$result$partition
bootstrap_clust[bootstrap_clust == 1] # Greece    Italy Portugal    Spain
bootstrap_clust[bootstrap_clust == 2] # Albania   Bulgaria    Romania Yugoslavia 
bootstrap_clust[bootstrap_clust == 3] # Denmark Finland  Norway  Swede
bootstrap_clust[bootstrap_clust == 4] # Austria     Belgium      France     Ireland Netherlands Switzerland          UK   W Germany 
bootstrap_clust[bootstrap_clust == 5] # Czechoslovakia      E Germany        Hungary         Poland           USSR

# of times each cluster has dissolved (more is bad)
k_bootstrap$bootbrd
# [1] 40 17 23 19 51

# stability of clusters (closer to 1 is good)
k_bootstrap$bootmean
# 0.703 0.835 0.842 0.758 0.606

# first and fifth aren't great.

clusplot(protein_matrix, k_bootstrap$result$partition, color=TRUE, shade=TRUE, labels=2, lines=0)
# incidentally, 1 and 5 are the most elongated on this graph

