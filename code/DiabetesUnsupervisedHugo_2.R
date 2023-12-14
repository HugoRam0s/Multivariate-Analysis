library(readr)
library(clustMixType)
library(cluster)  
library(factoextra)
library(ggplot2)
library(kmed)
library(FactoMineR)
library(PCAmixdata)
library(dplyr)
library(psych)
library(dbscan)
library(fpc)
library(scatterplot3d)
library(meanShiftR)
library(mclust)
library(tidyverse)

## Read dataset
diabetes <- read_csv("C:/Users/hugof/OneDrive/Documentos/GitHub/Multivariate-Analysis/data/processed/diabetes_PCA_2.csv",show_col_types = FALSE)
diabetes <- diabetes[,2:16]
diab <- diabetes[,2:15]
diabetes_long <- read_csv("C:/Users/hugof/OneDrive/Documentos/GitHub/Multivariate-Analysis/data/processed/preprocessed_data_diabetes.csv",show_col_types = FALSE)
diab_long <- diabetes_long[,3:23]
fviz_cluster(list(data = diabetes[, -which(names(diabetes) == "Diabetes_012")], cluster = diabetes$Diabetes_012))

#cols.ch <- c("HighBP","HighChol",'CholCheck', 'Smoker', 'Stroke','HeartDiseaseorAttack','PhysActivity','Fruits','Veggies','HvyAlcoholConsump','AnyHealthcare','NoDocbcCost','DiffWalk','Sex')
#diab_long[cols.ch] <- sapply(diab_long[cols.ch],as.character)
#diab_long[,c(4,14,15,16,19,20,21)] <- scale(diab_long[,c(4,14,15,16,19,20,21)])
#split<- splitmix(diab_long)
#pca.Pcamix <- PCAmix(X.quanti = split$X.quanti, X.quali = split$X.quali, ndim=14, rename.level = TRUE)
#diab <- as.data.frame(pca.Pcamix$ind$coord)

## Determining optimal clusters

# Elbow method
set.seed(123)
# function to compute total within-cluster sum of square 
wss <- function(k) {
  kmeans(diab, k, nstart = 10 )$tot.withinss
}

# Compute and plot wss for k = 1 to k = 15
k.values <- 1:20

# extract wss for 2-15 clusters
wss_values <- map_dbl(k.values, wss)

plot(k.values, wss_values,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")

# Silhouette method

# function to compute average silhouette for k clusters
avg_sil <- function(k) {
  km.res <- kmeans(diab, centers = k, nstart = 25)
  ss <- silhouette(km.res$cluster, dist(diab))
  mean(ss[, 3])
}

# Compute and plot wss for k = 1 to k = 15
k.values <- 2:15

# extract avg silhouette for 2-15 clusters
avg_sil_values <- map_dbl(k.values, avg_sil)

plot(k.values, avg_sil_values,
     type = "b", pch = 19, frame = FALSE, 
     xlab = "Number of clusters K",
     ylab = "Average Silhouettes")

#gap-statistic
set.seed(123)
gap_stat <- clusGap(diab, FUN = kmeans, nstart = 25,
                    K.max = 20, B = 8,verbose = TRUE)
# Print the result
print(gap_stat, method = "firstmax")
fviz_gap_stat(gap_stat)

## Clustering methods (K-prototypes/K-means, Hierarchical Clustering, Mixture models, DBSCAN, OPTICS)

# K-means

set.seed(123)  # For reproducibility
kmeans_result1 <- kmeans(diab_long, centers = 3)
cluster_assignments1 <- kmeans_result1$cluster-1
Kmeans_score1 <- (sum(diabetes_long$Diabetes_012 == cluster_assignments1)/nrow(diabetes_long))
contingency_table <- table(kmeans_result1$cluster, diabetes$Diabetes_012)
print(contingency_table)

set.seed(123)  # For reproducibility
kmeans_result2 <- kmeans(diab, centers = 3,nstart=25)
cluster_assignments2 <- kmeans_result2$cluster-1
Kmeans_score2 <- (sum(diabetes$Diabetes_012 == cluster_assignments2)/nrow(diabetes))
diab$cluster <- kmeans_result2$cluster-1
fviz_cluster(list(data = diab[, -which(names(diab) == "cluster")], cluster = diab$cluster))
contingency_table <- table(kmeans_result2$cluster, diabetes$Diabetes_012)
print(contingency_table)
diab <- diabetes[,2:15]

# K-medoids

set.seed(123)  # For reproducibility
kmedoids1 <- pam(diab_long,3,metric="manhattan",stand=FALSE)
cluster_assignments_kmed1 <- kmedoids1$clustering-1
Kmedoids_score1 <- (sum(diabetes_long$Diabetes_012 == cluster_assignments_kmed1)/nrow(diabetes_long))
contingency_table <- table(kmedoids1$clustering, diabetes$Diabetes_012)
print(contingency_table)


set.seed(123)  # For reproducibility
kmedoids2 <- pam(diab,3,metric="euclidean",stand=FALSE)
cluster_assignments_kmed2 <- kmedoids2$clustering-1
Kmedoids_score2 <- (sum(diabetes$Diabetes_012 == cluster_assignments_kmed2)/nrow(diabetes))
diab$cluster <- kmedoids2$cluster-1
fviz_cluster(list(data = diab[, -which(names(diab) == "cluster")], cluster = diab$cluster))
contingency_table <- table(kmedoids2$clustering, diabetes$Diabetes_012)
print(contingency_table)
diab <- diabetes[,2:15]

#split<- splitmix(diab_long)  #too expensive
#gower_dist <- distmix(diab_long, method = "gower", idnum = split$col.quant, idbin = split$col.qual)
#gower_kmed <- fastkmed(gower_dist,ncluster = 3,iterate = 5)
#cluster_assignments_kmed3 <- gower_kmed$cluster-1
#Kmedoids_score3 <- (sum(diabetes_long$Diabetes_012 == cluster_assignments_kmed3)/nrow(diabetes_long))

# Agglomerative clustering methods - too expensive

#cols.ch <- c("HighBP","HighChol",'CholCheck', 'Smoker', 'Stroke','HeartDiseaseorAttack','PhysActivity','Fruits','Veggies','HvyAlcoholConsump','AnyHealthcare','NoDocbcCost','DiffWalk','Sex')
#diab_long[cols.ch] <- sapply(diab_long[cols.ch],as.character)
#diab_long[,c(4,14,15,16,19,20,21)] <- scale(diab_long[,c(4,14,15,16,19,20,21)])
#split<- splitmix(diab_long)
#pca.Pcamix <- PCAmix(X.quanti = split$X.quanti, X.quali = split$X.quali, ndim=3, rename.level = TRUE)
#pca.Pcamix$eig
#diab_PCA <- pca.Pcamix$ind$coord

#a1_eu<-agnes(diab, metric = "euclidean",
                  #stand = FALSE, method = "single", keep.data = FALSE)

# DBSCAN

# Calculate k-distance plot
k_distances <- kNNdistplot(diab, k = 10)  # eps =3.5
abline(h = 3.5, lty = 2)

# Evaluate MinPts for different values
minPts_values <- 1:10  # Try different MinPts values
num_clusters <- numeric(length(minPts_values))

for (i in seq_along(minPts_values)) {
  dbscan_result <- dbscan(diab, eps = 3.5, MinPts = minPts_values[i])
  num_clusters[i] <- length(unique(dbscan_result$cluster[dbscan_result$cluster != 0]))
}

# Plot the number of clusters for different MinPts values
plot(minPts_values, num_clusters, type = "b", xlab = "MinPts", ylab = "Number of Clusters") #MinPts=3
# Choose the MinPts value where the number of clusters stabilizes or meets your criteria

dbscan_result <- dbscan(diab, eps = 3.5, MinPts = 3)
diab$cluster <- dbscan_result$cluster
fviz_cluster(list(data = diab[, -which(names(diab) == "cluster")], cluster = diab$cluster))
scatterplot3d(as.numeric(unlist(diab[, 1])), as.numeric(unlist(diab[, 2])), as.numeric(unlist(diab[, 3])), color = dbscan_result$cluster, pch = 19, main = "DBSCAN Clusters")
diab <- diabetes[,2:15]

# OPTICS encontra demasiados clusters

optics_result <- optics(diab, eps = 3.5, minPts = 3)
opt <- extractDBSCAN(optics_result, eps_cl = 1.5)
plot(opt)

# Mean shift Não funciona não percebo porquê

MN_result <- meanShift(diab, algorithm="KDTREE", alpha=0, iterations = 100)

# Gaussian mixture models

gmm_result <- Mclust(diab)
summary(gmm_result,parameters=TRUE)
plot(gmm_result, what = "classification")






