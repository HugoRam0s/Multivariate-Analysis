library(readr)
library(clustMixType)

## Read dataset
diabetes <- read_csv("C:/Users/hugof/OneDrive/Documentos/GitHub/Multivariate-Analysis/data/processed/preprocessed_data_diabetes.csv",show_col_types = FALSE)
diabetes <- diabetes[,2:23]
diab <- diabetes [,2:22]

## Clustering methods (K-prototypes/K-means, Hierarchical Clustering, Mixture models, DBSCAN, OPTICS)

# K-means

categorical_data <- diab[, c(14,19,20,21)]
categorical_data_factors <- lapply(categorical_data, factor)
encoded_categorical <- do.call(cbind, lapply(categorical_data_factors, function(x) model.matrix(~x - 1)))
numerical_cols <- diab[,c(4,15,16)]
scaled_numerical <- scale(numerical_cols)
combined_data1 <- cbind(scaled_numerical, encoded_categorical, diab[,c(1,2,3,5,6,7,8,9,10,11,12,13,17,18)])
kmeans_result1 <- kmeans(combined_data1, centers = 3)
cluster_assignments1 <- kmeans_result1$cluster-1
Kmean_score1 <- (sum(diabetes$Diabetes_012 == cluster_assignments1)/nrow(diabetes))




numerical_cols <- diab[,c(4,14,15,16,19,20,21)]
scaled_numerical <- scale(numerical_cols)
combined_data2 <- cbind(scaled_numerical, diab[,c(1,2,3,5,6,7,8,9,10,11,12,13,17,18)])

kmeans_result2 <- kmeans(combined_data2, centers = 3)
cluster_assignments2 <- kmeans_result2$cluster-1
Kmeans_score2 <- (sum(diabetes$Diabetes_012 == cluster_assignments2)/nrow(diabetes))


# silhouette visualization