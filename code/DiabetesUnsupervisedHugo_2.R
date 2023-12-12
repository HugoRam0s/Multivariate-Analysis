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

## Read dataset
diabetes <- read_csv("C:/Users/hugof/OneDrive/Documentos/GitHub/Multivariate-Analysis/data/processed/diabetes_PCA_2.csv",show_col_types = FALSE)
diabetes <- diabetes[,2:16]
diab <- diabetes[,2:15]
diabetes_long <- read_csv("C:/Users/hugof/OneDrive/Documentos/GitHub/Multivariate-Analysis/data/processed/preprocessed_data_diabetes.csv",show_col_types = FALSE)
diab_long <- diabetes_long[,3:23]

## Clustering methods (K-prototypes/K-means, Hierarchical Clustering, Mixture models, DBSCAN, OPTICS)

# K-means

set.seed(123)  # For reproducibility
kmeans_result1 <- kmeans(diab_long, centers = 3)
cluster_assignments1 <- kmeans_result1$cluster-1
Kmeans_score1 <- (sum(diabetes_long$Diabetes_012 == cluster_assignments1)/nrow(diabetes_long))

set.seed(123)  # For reproducibility
kmeans_result2 <- kmeans(diab, centers = 3)
cluster_assignments2 <- kmeans_result2$cluster-1
Kmeans_score2 <- (sum(diabetes$Diabetes_012 == cluster_assignments2)/nrow(diabetes))
diab$cluster <- kmeans_result2$cluster-1
fviz_cluster(list(data = diab[, -which(names(diab) == "cluster")], cluster = diab$cluster))

# K-medoids

set.seed(123)  # For reproducibility
kmedoids1 <- pam(diab_long,3,metric="manhattan",stand=FALSE)
cluster_assignments_kmed1 <- kmedoids1$clustering-1
Kmedoids_score1 <- (sum(diabetes_long$Diabetes_012 == cluster_assignments_kmed1)/nrow(diabetes_long))

set.seed(123)  # For reproducibility
kmedoids2 <- pam(diab,3,metric="euclidean",stand=FALSE)
cluster_assignments_kmed2 <- kmedoids2$clustering-1
Kmedoids_score2 <- (sum(diabetes$Diabetes_012 == cluster_assignments_kmed2)/nrow(diabetes))
diab$cluster <- kmedoids2$cluster-1
fviz_cluster(list(data = diab[, -which(names(diab) == "cluster")], cluster = diab$cluster))

#split<- splitmix(diab_long)  não funciona, é demasiado caro computacionalmente
#gower_dist <- distmix(diab_long, method = "gower", idnum = split$col.quant, idbin = split$col.qual)
#gower_kmed <- fastkmed(gower_dist,ncluster = 3,iterate = 5)
#cluster_assignments_kmed3 <- gower_kmed$cluster-1
#Kmedoids_score3 <- (sum(diabetes_long$Diabetes_012 == cluster_assignments_kmed3)/nrow(diabetes_long))

# Agglomerative clustering methods - too expensive

cols.ch <- c("HighBP","HighChol",'CholCheck', 'Smoker', 'Stroke','HeartDiseaseorAttack','PhysActivity','Fruits','Veggies','HvyAlcoholConsump','AnyHealthcare','NoDocbcCost','DiffWalk','Sex')
diab_long[cols.ch] <- sapply(diab_long[cols.ch],as.character)
diab_long[,c(4,14,15,16,19,20,21)] <- scale(diab_long[,c(4,14,15,16,19,20,21)])
split<- splitmix(diab_long)
pca.Pcamix <- PCAmix(X.quanti = split$X.quanti, X.quali = split$X.quali, ndim=3, rename.level = TRUE)
pca.Pcamix$eig
diab_PCA <- pca.Pcamix$ind$coord

a1_eu<-agnes(diab_PCA, metric = "euclidean",
                  stand = FALSE, method = "single", keep.data = FALSE)














#Agnes Function: The agnes() function from the cluster package can be memory-intensive
#Consider using a different clustering algorithm that is less memory-intensive, such as k-means or DBSCAN, 
# Ensure diab_PCA is a data frame

diab_PCA_df <- as.data.frame(diab_PCA)

#K-means Clustering

# Elbow Method to determine optimal number of clusters (k)
wss_values <- sapply(1:10, function(k) {
  kmeans_result <- kmeans(diab_PCA_df, centers = k, nstart = 25)
  kmeans_result$tot.withinss
})

# Plot the Elbow Method
plot(1:10, wss_values, type = "b", xlab = "Number of Clusters (k)", ylab = "Total Within-Cluster Sum of Squares", main = "Elbow Method for Optimal k")


#Elbow method suggests selecting the k at which the WSS begins to decrease at a slower rate. 
#Segundo o elbow metodo só vai haver dois clusters mas nós sabemos que há 3 classes por isso não sei se deíamos pôr 3
k <- 3

# Apply k-means clustering
set.seed(123)  # For reproducibility
kmeans_result <- kmeans(diab_PCA, centers = k)

# Add cluster assignments to the PCA data
diab_PCA_df$cluster <- kmeans_result$cluster

# Visualize the clusters
fviz_cluster(list(data = diab_PCA_df[, -which(names(diab_PCA_df) == "cluster")], cluster = diab_PCA_df$cluster))

