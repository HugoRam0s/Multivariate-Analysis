library(readr)
library(ggplot2)
library(GGally)
library(mvtnorm)
library(MVN)
library(energy)
library (dplyr)
library(stats)
library(rrcov)
## Read dataset
water <- read_csv('C:\\Users\\beatr\\OneDrive\\Documentos\\projetos_r\\Multivariate-Analysis\\data\\raw\\water_potability.csv', show_col_types = FALSE)
par(mfrow=c(3,3))
for (i in 1:9){
  boxplot(water[,i])
}
describe(water[,1:9])


for(i in names(water[,1:9])){
  hist(water[[i]], main = i, xlab = names(water)[i],breaks=50)
}

par(mfrow=c(1,1))
barplot(table(water[,10]), xlab='Potability category', ylab='count')

pairs.panels(water[,1:9], smooth = FALSE, scale = FALSE, density=TRUE,
             ellipses=FALSE,digits = 3,hist.col="blue")

ggpairs(water[,1:9], title="Correlogram")

water$Potability<-as.factor(water$Potability)
ggpairs(water[,1:9], ggplot2::aes(colour=water$Potability))

#Comparison between real data and random generated normal multivariate observations
n<-3276
set.seed(1738)
teoric<-rmvnorm(n,colMeans(water[,1:9]), cov(water[,1:9]))
colnames(teoric)<-colnames(water[,1:9])

print(round(cov(teoric),3))###Sample covariance matrix
print(round(cov(water[,1:9]),3)) ###Theoretical covariance matrix
round(cor(water[,1:9])-cor(teoric),3)

print(round(cor(teoric),3))###Sample correlation matrix
print(round(cor(water[,1:9]),3)) ###Theoretical correlation matrix
round(cor(water[,1:9])-cor(teoric),3)

## Using quantile-quantile plots and mahalanobis distance, multivariate normality tests

#Quantile-quantile of individual variables
par(mfrow=c(3,3))
for (i in colnames(water[,1:9])) { 
  qqnorm(water[[i]], main = i)
  qqline(water[[i]], col='red')
}


#Mardia's Multivariate Normality test
mvn(data = water[,1:9], mvnTest = "mardia")

#Henze-Zirkler Multivariate Normality test
mvn(data = water[,1:9], mvnTest = "hz")

#Doornik-Hansen Multivariate Normality test
mvn(data = water[,1:9], mvnTest = "dh")

# Energy test
mvnorm.etest(water[,1:9], R=100)

#Count the number of missing values
missing_values <- sapply(water[,1:10], function(x) sum(is.na(x)))
print(missing_values/3276*100)
barplot(missing_values/3276*100)

#%>% passes the result of the previous function (left) to the next function 
class_means <- water %>% 
  #Group the dataset by the class
  #The next functions will be applied to these subsets of the dataframe 
  group_by(Potability) %>%
  #Aplly 'mean()' to all columns of the grouped dataframe
  #~ creates a formula, and . represents each column in the dataframe
  summarise_all(~median(., na.rm = TRUE))

water_missing <- water

#Replace the missing values
for (col in names(water)[-length(names(water))]) {
  for (i in 1:nrow(water)) {
    if (is.na(water[[col]][i])) {
      if (water[['Potability']][i] == 0) {
        water_missing[[col]][i] <- class_means %>% 
          filter(Potability == 0) %>%
          pull(col) 
      } else {
        water_missing[[col]][i] <- class_means %>% 
          filter(Potability == 1) %>%
          pull(col)
      } 
    }
  }
}


par(mfrow=c(3,3))
for (i in 1:9){
  boxplot(water_missing[,i])
}
describe(water_missing[,1:9])
round(cov(water_missing[,1:9]),3)
round(cor(water_missing[,1:9]),3)

for(i in names(water_missing[,1:9])){
  hist(water_missing[[i]], main = i, xlab = names(water_missing)[i],breaks=50)
}


pairs.panels(water_missing[,1:9], smooth = FALSE, scale = FALSE, density=TRUE,
             ellipses=FALSE,digits = 3,hist.col="blue")

ggpairs(water_missing[,1:9], title="Correlogram")

water$Potability<-as.factor(water_missing$Potability)
ggpairs(water[,1:9], ggplot2::aes(colour=water_missing$Potability))

#Comparison between real data and random generated normal multivariate observations
n<-3276
set.seed(1738)
teoric_missing<-rmvnorm(n,colMeans(water_missing[,1:9]), cov(water_missing[,1:9]))
colnames(teoric_missing)<-colnames(water_missing[,1:9])

print(round(cov(teoric_missing),3))###Sample covariance matrix
print(round(cov(water_missing[,1:9]),3)) ###Theoretical covariance matrix
round(cor(water_missing[,1:9])-cor(teoric_missing),3)

print(round(cor(teoric_missing),3))###Sample correlation matrix
print(round(cor(water_missing[,1:9]),3)) ###Theoretical correlation matrix
round(cor(water_missing[,1:9])-cor(teoric_missing),3)

## Using quantile-quantile plots and mahalanobis distance, multivariate normality tests

#Quantile-quantile of individual variables
par(mfrow=c(3,3))
for (i in colnames(water_missing[,1:9])) { 
  qqnorm(water_missing[[i]], main = i)
  qqline(water_missing[[i]], col='red')
}


par(mfrow=c(1,1))
d<-mahalanobis(water_missing[,1:9], colMeans(water_missing[,1:9]), cov(water_missing[,1:9]))
d<-sort(d)
qqPlot(d,distribution = "chisq",df=ncol(water_missing[,1:9]),main="Quantile-quantile plot of mahalanobis distance",pch=20,col="red")


#Mardia's Multivariate Normality test
mvn(data = water_missing[,1:9], mvnTest = "mardia")

#Henze-Zirkler Multivariate Normality test
mvn(data = water_missing[,1:9], mvnTest = "hz")

#Doornik-Hansen Multivariate Normality test
mvn(data = water_missing[,1:9], mvnTest = "dh")

# Energy test
mvnorm.etest(water_missing[,1:9], R=100)

# Correlation PCA
data.cpca <- prcomp(water_missing[,1:9], scale. = T, retx=TRUE)
sum<-summary(data.cpca)
print(sum)
screeplot(data.cpca, main="PCA based on the Standardized Variables", type="lines",cex=0.8)
abline(h=1,col="red")


plot(sum[["importance"]][3,1:9], main="PCA based on the Standardized Variables", ylab = 'comulative variance', xlab='Principal components')
abline(h=0.8,col="red")

#
### Loadings
#
loadings<-data.cpca$r
print(round(loadings[,1:5] , digits = 3))
barplot(loadings[,1], xlab='variables', ylab='PC1')
barplot(loadings[,2], xlab='variables', ylab='PC2')
barplot(loadings[,3], xlab='variables', ylab='PC3')
barplot(loadings[,4], xlab='variables', ylab='PC4')
barplot(loadings[,5], xlab='variables', ylab='PC5')
#
### Scores
#
x <- data.cpca$x
### Correlation
#
round(cor(scale(water_missing[,1:9]),x)[,1:5],digits=3)
#
plot(x[, 1], x[, 2],col=water_missing$Potability, pch=1,xlab="PC1",ylab="PC2")
abline(h=0,col="black")
abline(v=0,col="black")
legend("topleft",legend = c("not Potable","Potable"),col=c("black","red"),pch=c(1,1))
plot(loadings[,1:2],   # x and y data
     pch=21,              # point shape
     bg="black",          # point color
     cex=1,               # point size
     main="Loadings"      # title of plot
)
text(loadings[,1:2],             # sets position of labels
     labels=rownames(loadings) )
abline(h=0,col="black")
abline(v=0,col="black")
plot(x[, 1], x[, 3],col=water_missing$Potability, pch=1,xlab="PC1",ylab="PC2")
legend("topleft",legend = c("not Potable","Potable"),col=c("black","red"),pch=c(1,1))
plot(x[, 1], x[, 4],col=water_missing$Potability, pch=1,xlab="PC1",ylab="PC2")
legend("topleft",legend = c("not Potable","Potable"),col=c("black","red"),pch=c(1,1))
plot(x[, 1], x[, 5],col=water_missing$Potability, pch=1,xlab="PC1",ylab="PC2")
legend("topleft",legend = c("not Potable","Potable"),col=c("black","red"),pch=c(1,1))
plot(x[, 2], x[, 3],col=water_missing$Potability, pch=1,xlab="PC1",ylab="PC2")
legend("topleft",legend = c("not Potable","Potable"),col=c("black","red"),pch=c(1,1))
plot(x[, 2], x[, 4],col=water_missing$Potability, pch=1,xlab="PC1",ylab="PC2")
legend("topleft",legend = c("not Potable","Potable"),col=c("black","red"),pch=c(1,1))
plot(x[, 2], x[, 5],col=water_missing$Potability, pch=1,xlab="PC1",ylab="PC2")
legend("topleft",legend = c("not Potable","Potable"),col=c("black","red"),pch=c(1,1))

# Robust PCA

water_preprocessed<-water_missing
for (i in colnames(water_preprocessed[,1:9])){
  water_preprocessed[[i]]<- (water_missing[[i]]-mean(water_missing[[i]]))/sd(water_missing[[i]])
}
data.robpca<-PcaHubert(water_preprocessed[,1:9], k=8)
print(data.robpca$loadings)
print(data.robpca$eigenvalues)
plot(data.robpca$eigenvalues, main="PCA based on the Standardized Variables",cex=0.8)
abline(h=1,col="red")


plot(cumsum(data.robpca$eigenvalues/data.robpca$totvar0), main="PCA based on the Standardized Variables", ylab = 'comulative variance', xlab='Principal components')
abline(h=0.8,col="red")
plot(data.robpca$scores[, 1], data.robpca$scores[, 2],col=water_missing$Potability, pch=1,xlab="PC1",ylab="PC2")
abline(h=0,col="black")
abline(v=0,col="black")
legend("topright",legend = c("not Potable","Potable"),col=c("black","red"),pch=c(1,1))
plot(data.robpca$loadings[,1:2],   # x and y data
     pch=21,              # point shape
     bg="black",          # point color
     cex=1,               # point size
     main="Loadings"      # title of plot
)
text(data.robpca$loadings[,1:2],             # sets position of labels
     labels=rownames(data.robpca$loadings) )
abline(h=0,col="black")
abline(v=0,col="black")

barplot(data.robpca$loadings[,1], xlab='variables', ylab='PC1')
barplot(data.robpca$loadings[,2], xlab='variables', ylab='PC2')
barplot(data.robpca$loadings[,3], xlab='variables', ylab='PC3')
barplot(data.robpca$loadings[,4], xlab='variables', ylab='PC4')
barplot(data.robpca$loadings[,5], xlab='variables', ylab='PC5')
