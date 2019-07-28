baseball=read.table("~/applied multivariate/assignment/baseball.final.csv",header=T, sep = ',')
names = read.csv("~/applied multivariate/assignment/PlayerNames.csv",  header = T)
head(baseball)
View(baseball)
colnames(baseball)
head(names)
summary(baseball)

baseball2 = baseball[,1:19]
baseball2
pairs(baseball2)
cor(baseball2)
baseball.pca = princomp(baseball2, cor = T)
summary(baseball.pca)
screeplot(baseball.pca)
plot(baseball.pca,type="l",main='Screeplot')

names(baseball.pca)
attributes(baseball.pca)
baseball.pca$sdev[1:7]     #std of PC
baseball.pca$sdev[1:7]^2   #eigenvalues
pca.loadings = baseball.pca$loadings[,1:7] #PC loadings 
pca.loadings

baseball.scores = (baseball.pca$scores[,1:7])
head(baseball.scores)
baseball.matrix = as.matrix(baseball2)
a = scale(baseball.scores, center = TRUE, scale = TRUE)

baseball.zscores = cbind(names, a)
head(baseball.zscores)
baseball.zscoresmatrix = as.matrix(a)

#mahalanobis distances
#uses pca scores
mahalanobis.comps = function(player = " "){
  a = which(names == player)
  d = baseball.zscoresmatrix[a,]
  y = mahalanobis(baseball.zscoresmatrix, center = d, cov = var(baseball.zscoresmatrix))
  e =  order(y)
  f = baseball.zscores[c(e[1:11]),]
  return(f)
}
mahalanobis.comps(player = "Ryan Howard 2008")

#cluster analysis
dim(baseball.matrix)
variable_min = apply(baseball.matrix,2,min)
variable_min
variable_max = apply(baseball.matrix,2,max)
variable_max
means = apply(baseball.matrix,2,mean)
# Set up blank plot for profiles
plot(c(0,19),c(0,1),type="n",xlab="variables",ylab="Value",main="Profile Plot")
# Use a loop to generate a profile line for each observation.
for (k in (1:4020))
{
  points(1:19,baseball.matrix[k,],type="l")
  
}
points(1:19, means, type ="b", col="red" )
points(1:19, baseball.matrix[376,], type = "b", col = "green")
points(1:19, baseball.matrix[377,], type = 'b',col = "green")
points(1:19, baseball.matrix[378,], type = 'b', col = "green")

# Hierarchical clustering of the crab data.
# The hclust computes the cluster object.
baseball.clus <- hclust(dist(baseball2), method="complete") 
baseball.clus
summary(baseball.clus)
# specifed (k) number of clusters.
baseball.kclusters <- cutree(baseball.clus,k=7)
baseball.kclusters
# You might want to see how many observations in each cluster.
table(baseball.kclusters)
which(baseball.kclusters==7, arr.ind = TRUE)
baseball.zscores[c(376,377,378),]
baseball[c(376,377,378),]
clusplot(baseball,baseball.kclusters,stand=TRUE,labels=3,main="Baseball complete link")

# K means cluster method.
# Determine number of clusters
wss <- (nrow(baseball)-1)*sum(apply(baseball,2,var))
for (i in 2:20) wss[i] <- sum(kmeans(baseball,centers=i)$withinss)
plot(1:20, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")
baseball.pkcl <- kmeans(baseball.pca$scores,7,20)
plot(baseball.pca$scores[,1:7], col = baseball.pkcl$cluster)
title ("K Means Cluster on first two PCs - Baseball Data")
clusplot(baseball,baseball.pkcl$cluster,stand=TRUE,labels=2,main="Baseball-kmeans, 7 clusters")

#Determine means of z-scores for each component for each cluster
cluster.vector = baseball.kcl$cluster
clus_1 =which(baseball.kcl$cluster==1, arr.ind = TRUE)
clus_1_z.scores = baseball.zscores[c(clus_1),]
cluster1_mean = apply(clus_1_z.scores[,c(2:8)], 2, mean)
clus_2= which(baseball.kcl$cluster == 2, arr.ind = TRUE)
clus_2_z.scores = baseball.zscores[c(clus_2),]
cluster2_mean = apply(clus_2_z.scores[,c(2:8)],2, mean)
clus_3= which(baseball.kcl$cluster == 3, arr.ind = TRUE)
clus_3_z.scores = baseball.zscores[c(clus_3),]
cluster3_mean = apply(clus_3_z.scores[,c(2:8)],2, mean)
clus_4= which(baseball.kcl$cluster == 4, arr.ind = TRUE)
clus_4_z.scores = baseball.zscores[c(clus_4),]
cluster4_mean = apply(clus_4_z.scores[,c(2:8)],2, mean)
clus_5= which(baseball.kcl$cluster == 5, arr.ind = TRUE)
clus_5_z.scores = baseball.zscores[c(clus_5),]
cluster5_mean = apply(clus_5_z.scores[,c(2:8)],2, mean)
clus_6= which(baseball.kcl$cluster == 6, arr.ind = TRUE)
clus_6_z.scores = baseball.zscores[c(clus_6),]
cluster6_mean = apply(clus_6_z.scores[,c(2:8)],2, mean)
clus_7= which(baseball.kcl$cluster == 7, arr.ind = TRUE)
clus_7_z.scores = baseball.zscores[c(clus_7),]
cluster7_mean =apply(clus_7_z.scores[,c(2:8)],2, mean)
cluster_z_scores = rbind(cluster1_mean,cluster2_mean,cluster3_mean, cluster4_mean, cluster5_mean, cluster6_mean, cluster7_mean)
cluster_z_scores
baseball.with.clusters = cbind(names,baseball2,cluster.vector)
dim(baseball.with.clusters)
head(baseball.with.clusters)

#Decision tree on the clusters
library(tree)
observation_cluster = as.factor(cluster.vector)
baseball.tree = tree(observation_cluster ~ O.Swing. + Z.Swing. + Swing. + O.Contact. + Z.Contact. + Contact. + Zone. + F.Strike. + SwStr. + BB. + K. + ISO + BABIP + LD. + GB. + FB. + IFFB. + HR.FB + IFH., baseball.with.clusters)
summary(baseball.tree)
baseball.tree
plot(baseball.tree)                  
text(baseball.tree,srt=0,adj=1)             
title("Baseball Tree")
cv.baseball.tree <- cv.tree(baseball.tree, ,prune.tree)
plot(cv.baseball.tree,type="b")
pruned.baseball.tree <- prune.tree(baseball.tree,best=9)
plot(pruned.baseball.tree)
text(pruned.baseball.tree, srt = 0, adj = 1)
title("Final Baseball Tree")
summary(pruned.baseball.tree)


