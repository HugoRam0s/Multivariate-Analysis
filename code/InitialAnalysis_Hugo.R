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
water <- read_csv('C:/Users/hugof/OneDrive/Documentos/GitHub/Multivariate-Analysis/data/processed/cleaned_data.csv',show_col_types = FALSE)

## Preprocessing
# Check how much percentage of data is missing
Na <- (nrow(water [!complete.cases(water), ])/3276)*100
Na_ph <- (nrow(water [!complete.cases(water$ph), ])/3276)*100 
Na_Sulphate <- (nrow(water [!complete.cases(water$Sulfate), ])/3276)*100 
Na_Trihalo <- (nrow(water [!complete.cases(water$Trihalomethanes), ])/3276)*100
# Fill Na values with the median of the respective variable
#water1 <- na.aggregate(water)
#water <- water1
#write.csv(water, "C:/Users/hugof/OneDrive/Documentos/GitHub/Multivariate-Analysis/data/processed/waterProcessed.csv", row.names=TRUE)

## Basic statistic analysis in tables

summary(water[,2:10])
describe(water[,2:10])
round(var(water[,2:10]),3)
round(cov(water[,2:10]),3)
round(cor(water),3)

## Data visualization

for(i in names(water[,2:10])){
  hist(water[[i]], main = i, xlab = names(water)[i],breaks=50)
}

pairs.panels(water[,2:10], smooth = FALSE, scale = FALSE, density=TRUE,
             ellipses=FALSE,digits = 2,hist.col="green")

ggpairs(water[,2:10], title="Correlogram")

water$Potability<-as.factor(water$Potability)
ggpairs(water[,2:10], ggplot2::aes(colour=water$Potability))

## Checking for normality 

#Comparison between real data and random generated normal multivariate observations
n<-10000
set.seed(1738)
teoric<-rmvnorm(n,colMeans(water[,2:10]), cov(water[,2:10]))
colnames(teoric)<-colnames(water[,2:10])

print(round(cov(teoric),3))###Sample covariance matrix
print(round(cov(water[,2:10]),3)) ###Theoretical covariance matrix
round(cor(water[,2:10])-cor(teoric),3)

print(round(cor(teoric),3))###Sample correlation matrix
print(round(cor(water[,2:10]),3)) ###Theoretical correlation matrix
round(cor(water[,2:10])-cor(teoric),3)

## Using quantile-quantile plots and mahalanobis distance, multivariate normality tests

#Quantile-quantile of individual variables
qqnorm(water$ph)
qqline(water$ph)
qqnorm(water$Hardness)
qqline(water$Hardness)
qqnorm(water$Solids)
qqline(water$Solids)
qqnorm(water$Chloramines)
qqline(water$Chloramines)
qqnorm(water$Sulfate)
qqline(water$Sulfate)
qqnorm(water$Conductivity)
qqline(water$Conductivity)
qqnorm(water$Organic_carbon)
qqline(water$Organic_carbon)
qqnorm(water$Trihalomethanes)
qqline(water$Trihalomethanes)
qqnorm(water$Turbidity)
qqline(water$Turbidity)

#Quantile plots of mahalanobis distance
d<-mahalanobis(water[,2:10], colMeans(water[,2:10]), cov(water[,2:10]))
d<-sort(d)
qqPlot(d,distribution = "chisq",df=ncol(water[,2:10]),main="Quantile-quantile plot of mahalanobis distance",pch=20,col="red")


#Mardia's Multivariate Normality test
mvn(data = water[,2:10], mvnTest = "mardia")

#Henze-Zirkler Multivariate Normality test
mvn(data = water[,2:10], mvnTest = "hz")

#Doornik-Hansen Multivariate Normality test
mvn(data = water[,2:10], mvnTest = "dh")

# Energy test
mvnorm.etest(water[,2:10], R=100)
