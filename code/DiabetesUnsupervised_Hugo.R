library(readr)
library(clustMixType)
library(cluster)  
library(factoextra)
library(ggplot2)
library(kmed)
library(FactoMineR)
library(PCAmixdata)
library(dplyr)

## Read dataset
diabetes <- read_csv("C:/Users/hugof/OneDrive/Documentos/GitHub/Multivariate-Analysis/data/processed/preprocessed_data_diabetes.csv",show_col_types = FALSE)
diabetes <- diabetes[,2:23]
diab <- diabetes [,2:22]

## Perform dimension reduction for more computationally expensive methods

cols.ch <- c("HighBP","HighChol",'CholCheck', 'Smoker', 'Stroke','HeartDiseaseorAttack','PhysActivity','Fruits','Veggies','HvyAlcoholConsump','AnyHealthcare','NoDocbcCost','DiffWalk','Sex')
diab[cols.ch] <- sapply(diab[cols.ch],as.character)
diab_sc<-diab %>% mutate(across(where(is.numeric), scale))

split<- splitmix(diab)
pca.famd <- FAMD(diab, ncp = 10, graph = FALSE) 
get_eigenvalue(pca.famd)
fviz_screeplot(pca.famd)
pca.Pcamix <- PCAmix(X.quanti = split$X.quanti, X.quali = split$X.quali, ndim=10, rename.level = TRUE)
pca.Pcamix$eig
diab_PCA <- pca.Pcamix$ind$coord

## Clustering methods (K-prototypes/K-means, Hierarchical Clustering, Mixture models, DBSCAN, OPTICS)

# K-means

numerical_cols <- diab[,c(4,14,15,16,19,20,21)]
scaled_numerical <- scale(numerical_cols)
combined_data2 <- cbind(scaled_numerical, diab[,-c(4,14,15,16,19,20,21)])

kmeans_result1 <- kmeans(diab, centers = 3)
cluster_assignments1 <- kmeans_result1$cluster-1
Kmeans_score1 <- (sum(diabetes$Diabetes_012 == cluster_assignments1)/nrow(diabetes))

kmeans_result2 <- kmeans(diab_PCA, centers = 3)
cluster_assignments2 <- kmeans_result2$cluster-1
Kmeans_score2 <- (sum(diabetes$Diabetes_012 == cluster_assignments2)/nrow(diabetes))

# K-medoids

kmedoids1 <- pam(diab,3,metric="manhattan",stand=FALSE)
cluster_assignments_kmed1 <- kmedoids1$clustering-1
Kmedoids_score1 <- (sum(diabetes$Diabetes_012 == cluster_assignments_kmed1)/nrow(diabetes))

kmedoids2 <- pam(diab_PCA,3,metric="euclidean",stand=FALSE)
cluster_assignments_kmed2 <- kmedoids2$clustering-1
Kmedoids_score2 <- (sum(diabetes$Diabetes_012 == cluster_assignments_kmed2)/nrow(diabetes))

gower_dist <- distmix(diab, method = "gower", idnum = split$col.quant, idbin = split$col.qual)
gower_kmed <- fastkmed(gower_dist,ncluster = 3,iterate = 5)
cluster_assignments_kmed3 <- gower_kmed$cluster-1
Kmedoids_score3 <- (sum(diabetes$Diabetes_012 == cluster_assignments_kmed3)/nrow(diabetes))

# silhouette visualization
