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
library(NbClust)

## Read datasets

# Dimension reduced dataset
diabetes <- read_csv("C:/Users/hugof/OneDrive/Documentos/GitHub/Multivariate-Analysis/data/processed/diabetes_PCA_2.csv",show_col_types = FALSE)
diabetes <- diabetes[,2:16] #labeled dataset
diab <- diabetes[,2:15] #unlabeled dataset

# Original dataset pre-processed
diabetes_long <- read_csv("C:/Users/hugof/OneDrive/Documentos/GitHub/Multivariate-Analysis/data/processed/preprocessed_data_diabetes.csv",show_col_types = FALSE)
diab_long <- diabetes_long[,3:23] #unlabeled dataset

# scale of numeric variables from the original dataset
cols.ch <- c("HighBP","HighChol",'CholCheck', 'Smoker', 'Stroke','HeartDiseaseorAttack','PhysActivity','Fruits','Veggies','HvyAlcoholConsump','AnyHealthcare','NoDocbcCost','DiffWalk','Sex')
diab_long[cols.ch] <- sapply(diab_long[cols.ch],as.character)
diab_long[,c(4,14,15,16,19,20,21)] <- scale(diab_long[,c(4,14,15,16,19,20,21)])

# Visualization of cluster ground truth from dimension reduced dataset
fviz_cluster(list(data = diabetes[, -which(names(diabetes) == "Diabetes_012")], cluster = diabetes$Diabetes_012))
scatterplot3d(as.numeric(unlist(diabetes[, 2])), as.numeric(unlist(diabetes[, 3])), as.numeric(unlist(diabetes[, 4])), color = diabetes$Diabetes_012, pch = 19, main = "Ground truth clusters")

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

# Compute and plot wss for k = 1 to k = 10
k.values <- 1:10

# extract wss for 2-10 clusters
wss_values <- map_dbl(k.values, wss)

#vizualization of wss for 2-10 clusters
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

# Compute and plot wss for k = 1 to k = 10
k.values <- 2:10

# extract avg silhouette for 2-10 clusters
avg_sil_values <- map_dbl(k.values, avg_sil)

plot(k.values, avg_sil_values,
     type = "b", pch = 19, frame = FALSE, 
     xlab = "Number of clusters K",
     ylab = "Average Silhouettes")

#gap-statistic
set.seed(123)
gap_stat <- clusGap(diab, FUN = kmeans, nstart = 25,
                    K.max = 15, B = 8,verbose = TRUE)
# Print the result
print(gap_stat, method = "firstmax")
fviz_gap_stat(gap_stat)

# Calinski — Harabasz Method
fviz_ch <- function(data) {
  ch <- c()
  for (i in 2:10) {
    km <- kmeans(data, i) # perform clustering
    ch[i] <- calinhara(data, # data
                       km$cluster, # cluster assignments
                       cn=max(km$cluster) # total cluster number
    )
  }
  ch <-ch[2:10]
  k <- 2:10
  plot(k, ch,xlab =  "Cluster number k",
       ylab = "Caliński - Harabasz Score",
       main = "Caliński - Harabasz Plot", cex.main=1,
       col = "dodgerblue1", cex = 0.9 ,
       lty=1 , type="o" , lwd=1, pch=4,
       bty = "l",
       las = 1, cex.axis = 0.8, tcl  = -0.2)
  abline(v=which(ch==max(ch)) + 1, lwd=1, col="red", lty="dashed")
}

fviz_ch(diab)

#Davies — Bouldin Method

fviz_db <- function(data) {
  k <- c(2:20)
  nb <- NbClust(data, min.nc = 2, max.nc = 20, index = "db", method = "kmeans")
  db <- as.vector(nb$All.index)
  plot(k, db,xlab =  "Cluster number k",
       ylab = "Davies-Bouldin Score",
       main = "Davies-Bouldin Plot", cex.main=1,
       col = "dodgerblue1", cex = 0.9 ,
       lty=1 , type="o" , lwd=1, pch=4,
       bty = "l",
       las = 1, cex.axis = 0.8, tcl  = -0.2)
  abline(v=which(db==min(db)) + 1, lwd=1, col="red", lty="dashed")
}


fviz_db(diab)

## Clustering methods (K-prototypes/K-means, Hierarchical Clustering, Mixture models, DBSCAN, OPTICS)

## K-means

# K-means on the original dataset 
set.seed(123)  # For reproducibility
kmeans_result1 <- kmeans(diab_long, centers = 3, nstart=50) #Apply k-means
cluster_assignments1 <- kmeans_result1$cluster-1 #Transform 1,2,3 to 0,1,2
Kmeans_score1 <- (sum(diabetes_long$Diabetes_012 == cluster_assignments1)/nrow(diabetes_long)) #Score correspondence between clusters and ground truth
contingency_table <- table(kmeans_result1$cluster, diabetes$Diabetes_012) #Contingency table to check correspondence
print(contingency_table)

# K-means on the dimension reduced dataset
set.seed(123)  # For reproducibility
kmeans_result2 <- kmeans(diab, centers = 3,nstart=50) #Apply k-means
cluster_assignments2 <- kmeans_result2$cluster-1 #Transform 1,2,3 to 0,1,2
Kmeans_score2 <- (sum(diabetes$Diabetes_012 == cluster_assignments2)/nrow(diabetes)) #Score correspondence between clusters and ground truth

#Visualization of clusters
diab$cluster <- kmeans_result2$cluster-1
fviz_cluster(list(data = diab[, -which(names(diab) == "cluster")], cluster = diab$cluster))

contingency_table <- table(kmeans_result2$cluster, diabetes$Diabetes_012) #Contingency table to check correspondence
print(contingency_table)
diab <- diabetes[,2:15]

## K-medoids

# K-medoids on the original dataset 
set.seed(123)  # For reproducibility
kmedoids1 <- pam(diab_long,3,metric="manhattan",stand=FALSE) #PAM using manhattan dissimilarity
cluster_assignments_kmed1 <- kmedoids1$clustering-1 #Transform 1,2,3 to 0,1,2
Kmedoids_score1 <- (sum(diabetes_long$Diabetes_012 == cluster_assignments_kmed1)/nrow(diabetes_long)) #Score correspondence between clusters and ground truth
contingency_table <- table(kmedoids1$clustering, diabetes$Diabetes_012) #Contingency table to check correspondence
print(contingency_table)

set.seed(123)  # For reproducibility
kmedoids2 <- pam(diab_long,3,metric="euclidean",stand=FALSE) #PAM using euclidean dissimilarity
cluster_assignments_kmed2 <- kmedoids2$clustering-1 #Transform 1,2,3 to 0,1,2
Kmedoids_score2 <- (sum(diabetes_long$Diabetes_012 == cluster_assignments_kmed2)/nrow(diabetes_long)) #Score correspondence between clusters and ground truth
contingency_table <- table(kmedoids2$clustering, diabetes$Diabetes_012) #Contingency table to check correspondence
print(contingency_table)

# K-medoids on the dimension reduced dataset
set.seed(123)  # For reproducibility
kmedoids3 <- pam(diab,3,metric="euclidean",stand=FALSE) #PAM using euclidean dissimilarity
cluster_assignments_kmed3 <- kmedoids3$clustering-1 #Transform 1,2,3 to 0,1,2
Kmedoids_score3 <- (sum(diabetes$Diabetes_012 == cluster_assignments_kmed3)/nrow(diabetes))
diab$cluster <- kmedoids3$cluster-1
fviz_cluster(list(data = diab[, -which(names(diab) == "cluster")], cluster = diab$cluster))#Score correspondence between clusters and ground truth
contingency_table <- table(kmedoids3$clustering, diabetes$Diabetes_012) #Contingency table to check correspondence
print(contingency_table)
diab <- diabetes[,2:15]

set.seed(123)  # For reproducibility
kmedoids4 <- pam(diab,3,metric="manhattan",stand=FALSE) #PAM using manhattan dissimilarity
cluster_assignments_kmed4 <- kmedoids4$clustering-1 #Transform 1,2,3 to 0,1,2
Kmedoids_score4 <- (sum(diabetes$Diabetes_012 == cluster_assignments_kmed4)/nrow(diabetes))
diab$cluster <- kmedoids4$cluster-1
fviz_cluster(list(data = diab[, -which(names(diab) == "cluster")], cluster = diab$cluster)) #Score correspondence between clusters and ground truth
contingency_table <- table(kmedoids4$clustering, diabetes$Diabetes_012)#Contingency table to check correspondence
print(contingency_table)
diab <- diabetes[,2:15]

# K-medoids on the original dataset using gower distance
split<- splitmix(diab_long)  #Get binary and numeric variables
gower_dist <- distmix(diab_long, method = "gower", idnum = split$col.quant, idbin = split$col.qual) #Get gower distance
gower_kmed <- fastkmed(gower_dist,ncluster = 3,iterate = 5) # Apply k-medoids
cluster_assignments_kmed5 <- gower_kmed$cluster-1 #Transform 1,2,3 to 0,1,2
Kmedoids_score5 <- (sum(diabetes_long$Diabetes_012 == cluster_assignments_kmed5)/nrow(diabetes_long))
contingency_table <- table(gower_kmed$cluster, diabetes$Diabetes_012) #Contigency table to check correspondences
print(contingency_table)

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

dbscan_result <- dbscan(diab, eps = 3.5, MinPts = 3) #Apply DBSCAN
diab$cluster <- dbscan_result$cluster
fviz_cluster(list(data = diab[, -which(names(diab) == "cluster")], cluster = diab$cluster)) #Visualize cluster assignment along the most significant dimensions
contingency_table <- table(diab$cluster, diabetes$Diabetes_012)#Contigency table to check dimensions
print(contingency_table)
diab <- diabetes[,2:15]

# OPTICS - finds too many clusters, not used

optics_result <- optics(diab, eps = 3.5, minPts = 3) #Apply OPTICS with the same parameters as DBSCAN
opt <- extractDBSCAN(optics_result, eps_cl = 1.5) #Extract cluster assignments
plot(opt) #Visualize cluster assignments

# Gaussian mixture models

gmm_result <- Mclust(diab) #Apply gaussian mixture model clustering
summary(gmm_result,parameters=TRUE)
diab$cluster <- gmm_result$classification
contingency_table <- table(diab$cluster, diabetes$Diabetes_012) #Conteigency table to check correspondences
print(contingency_table)
diab <- diabetes[,2:15]
plot(gmm_result, what = "classification") #Visualize cluster assignments in all dimensions 2D

# Export the best clustering technique ---> k-medoids in the original dataset

diab$cluster <- kmedoids2$clustering
write.csv(diab, "C:/Users/hugof/OneDrive/Documentos/GitHub/Multivariate-Analysis/data/processed/Unsupervised_labeled.csv", row.names=FALSE)
diab <- diabetes[,2:15]





