library(readr)
library(psych)
library(zoo)
library(ggplot2)
library(GGally)
library(MVN)
library(energy)
library(mvtnorm)
library(car)

## Read dataset
diabetes <- read_csv("C:/Users/hugof/OneDrive/Documentos/GitHub/Multivariate-Analysis/data/processed/preprocessed_data_diabetes.csv",show_col_types = FALSE)
diabetes <- diabetes[,2:23]

#Remove duplicate rows
diabetes <- diabetes[!duplicated(diabetes), ]

## Basic statistic analysis in tables

summary(diabetes[,2:22])
describe(diabetes[,2:22])
round(var(diabetes[,2:22]),3)
round(cov(diabetes[,2:22]),3)
round(cor(diabetes),3)

## Data visualization

slices <- as.vector(table(diabetes$Diabetes_012))
lbls <- names(table(diabetes$Diabetes_012))
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct) # add percents to labels
lbls <- paste(lbls,"%",sep="") # ad % to labels
pie(slices,labels = lbls, col=rainbow(length(lbls)),
    main="Diabetes diagnosis")

for(i in names(diabetes[,2:22])){
  hist(diabetes[[i]], main = i, xlab = names(diabetes)[i],breaks=50)
}

pairs.panels(diabetes[,17:22], smooth = FALSE, scale = FALSE, density=TRUE,
             ellipses=FALSE,digits = 2,hist.col="green")

ggpairs(diabetes[,17:22], title="Correlogram")

diabetes$Diabetes_012<-as.factor(diabetes$Diabetes_012)
ggpairs(diabetes[,17:22], ggplot2::aes(colour=diabetes$Diabetes_012))

ggcorr(diabetes, method = c("everything", "pearson"))

#Remove low correlation variables
cor_matrix <- cor(diabetes_u)
column_ids <- which(abs(cor_matrix[1,]) > 0.1)
diabetes_filt <- diabetes_u[,column_ids]

## Checking for normality 

## Using quantile-quantile plots and mahalanobis distance, multivariate normality tests

#Quantile-quantile of individual variables
qqnorm(diabetes$BMI)
qqline(diabetes$BMI)

#Quantile plots of mahalanobis distance
d<-mahalanobis(diabetes[,2:22], colMeans(diabetes[,2:22]), cov(diabetes[,2:22]))
d<-sort(d)
qqPlot(d,distribution = "chisq",df=ncol(diabetes[,2:22]),main="Quantile-quantile plot of mahalanobis distance",pch=20,col="red")


#Mardia's Multivariate Normality test (null hypotesis: the dataset follows a MVN distribuition)
mvn(data = diabetes[,2:22], mvnTest = "mardia")

#Henze-Zirkler Multivariate Normality test
mvn(data = diabetes[,2:22], mvnTest = "hz")

#Doornik-Hansen Multivariate Normality test
mvn(data = diabetes[,2:22], mvnTest = "dh")
