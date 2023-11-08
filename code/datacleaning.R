library (dplyr)

df<- water_potability

#Count the number of missing values
missing_values <- sapply(df, function(x) sum(is.na(x)))

#Determine the initial variable's mean
variable_mean_initial <- colMeans(df, na.rm = TRUE)
print(variable_mean_initial)

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

#Fill the missing values with the respective variable's mean of the respective class
#Calculat the variable's mean for class 0 and 1

#%>% passes the result of the previous function (left) to the next function 
class_means <- df %>% 
  #Group the dataset by the class
  #The next functions will be applied to these subsets of the dataframe 
  group_by(Potability) %>%
  #Aplly 'mean()' to all columns of the grouped dataframe
  #~ creates a formula, and . represents each column in the dataframe
  summarise_all(~mean(., na.rm = TRUE))

#Replace the missing values
for (col in names(df)[-length(names(df))]) {
  for (i in 1:nrow(df)) {
    if (is.na(df[[col]][i])) {
      if (df[['Potability']][i] == 0) {
        df[[col]][i] <- class_means %>% 
          filter(Potability == 0) %>%
          pull(col) 
      } else {
        df[[col]][i] <- class_means %>% 
          filter(Potability == 1) %>%
          pull(col)
      } 
    }
  }
}

#Determine the variable's mean after cleaning the data
variable_mean_clean <- colMeans(df, na.rm = TRUE)
print(variable_mean_clean)










