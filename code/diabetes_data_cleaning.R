library (dplyr)
diabetes <- read.csv("/Users/beatrizrebelo/Downloads/diabetes_012_health_indicators_BRFSS2015.csv")
df<- diabetes
head(df)

#Count the number of missing values
missing_values <- sapply(df, function(x) sum(is.na(x)))
print(missing_values)

#Determine the initial variable's mean
variable_mean_initial <- colMeans(df, na.rm = TRUE)
print(variable_mean_initial)

#Determine the initial variable's standard deviation
variable_sd_initial <- sapply(df, sd, na.rm = TRUE)
print(variable_sd_initial)

#Identify and remove outliers (assumed to be missing values)
# Initialize a vector to store the number of outliers for each column except the last one (class)
outliers_count <- numeric(length(names(df)) - 1)

#Iterates on the columns, except the last one (class)
for (i in 1:(length(names(df)) - 1)) { 
  col <- names(df)[i]
  if (is.numeric(df[[col]])) {
    #Determine the 1st and 3rd quartile, ignoring the missing values
    Q1 <- quantile(df[[col]], 0.25, na.rm = TRUE)
    Q3 <- quantile(df[[col]], 0.75, na.rm = TRUE)
    IQR <- Q3 - Q1
    #Define the decision boundary
    lower_bound <- Q1 - 1.5 * IQR
    upper_bound <- Q3 + 1.5 * IQR
    #Find the outliers
    outliers <- which(df[[col]] < lower_bound | df[[col]] > upper_bound)
    #Count the outliers
    outliers_count[i] <- length(outliers)
    # Replace outliers with NA
    df[[col]][outliers] <- NA
  } else {
    # Assign NA for non-numeric columns
    outliers_count[i] <- NA  
  }
}

#Count the number of missing values - just outliers because there aren't missing values
missing_values <- sapply(df, function(x) sum(is.na(x)))
print(missing_values)

#Determine the variable's mean without the outliers
variable_mean <- colMeans(df, na.rm = TRUE)
print(variable_mean_initial)

#Determine the variable's standard deviation without the outliers
variable_sd <- sapply(df, sd, na.rm = TRUE)
print(variable_sd_initial)



library(dplyr)

table(diabetes$Diabetes_012)
class_0 <- diabetes %>% filter(Diabetes_012 == 0)
class_1 <- diabetes %>% filter(Diabetes_012 == 1)
class_2 <- diabetes %>% filter(Diabetes_012 == 2)

class_counts <- table(diabetes$Diabetes_012)
min_class_size <- min(class_counts)

set.seed(123) 

class_0_downsampled <- sample_n(class_0, min_class_size)
class_1_downsampled <- class_1
class_2_downsampled <- sample_n(class_2, min_class_size)

balanced_data <- rbind(class_0_downsampled, class_1_downsampled, class_2_downsampled)

table(balanced_data$Diabetes_012)


write.csv(diabetes, file = "/Users/beatrizrebelo/Desktop/Mestrado/preprocessed_data.csv", row.names = TRUE)