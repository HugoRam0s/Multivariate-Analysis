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
diabetes2 <- read_csv('C:/Users/hugof/Downloads/diabetes_012_health_indicators_BRFSS2015.csv',show_col_types = FALSE)
diabetes <- read_csv("C:/Users/hugof/Downloads/diabetes_binary_5050split_health_indicators_BRFSS2015.csv",show_col_types = FALSE)
diabetes3 <- read_csv("C:/Users/hugof/Downloads/diabetes_binary_health_indicators_BRFSS2015.csv",show_col_types = FALSE)


## Basic statistic analysis in tables

summary(diabetes[,2:22])
describe(diabetes[,2:22])
round(var(diabetes[,2:22]),3)
round(cov(diabetes[,2:22]),3)
round(cor(diabetes),3)

## Data visualization

for(i in names(diabetes[,2:22])){
  hist(diabetes[[i]], main = i, xlab = names(diabetes)[i],breaks=50)
}

pairs.panels(diabetes[,2:22], smooth = FALSE, scale = FALSE, density=TRUE,
             ellipses=FALSE,digits = 2,hist.col="green")

ggpairs(diabetes[,2:22], title="Correlogram")

diabetes$Potability<-as.factor(diabetes$Diabetes_012)
ggpairs(diabetes[,2:22], ggplot2::aes(colour=diabetes$Diabetes_012))

ggcorr(diabetes, method = c("everything", "pearson"))

## Checking for normality 

#Comparison between real data and random generated normal multivariate observations
#n<-10000
#set.seed(1738)
#teoric<-rmvnorm(n,colMeans(diabetes[,2:22]), cov(diabetes[,2:22]))
#colnames(teoric)<-colnames(diabetes[,2:22])

#round(cov(diabetes[,2:22])-cov(teoric),3)

#round(cor(diabetes[,2:22])-cor(teoric),3)

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
