
#1 
df1 <- read.csv("C:/Users/User/Downloads/winequality-red.csv",sep=';')
#df1
df2 <- read.csv("C:/Users/User/Downloads/winequality-white.csv",sep = ';')


#2 Now add a label column to both df1 and df2 indicating a label 'red' or 'white'.


head(df1)
head(df2)
library(dplyr)


df1$label <- sapply(df1$pH,function(x){'red'})
df2$label <- sapply(df2$pH,function(x){'white'})


#3 Check the head of df1 and df2

head(df1)
head(df2)


#4 Combine df1 and df2 into a single data frame called wine.


wine <- rbind(df1,df2)

wine <- rbind(df2,df1)

head(wine)

tail(wine)



#5 Create a Histogram of residual sugar from the wine data. Color by red and white wines

library(ggplot2)

ggplot(wine, aes(x=residual.sugar)) + 
  geom_histogram(binwidth=1)



pl <- ggplot(wine,aes(x=residual.sugar)) + 
  geom_histogram(aes(fill=label),color='black',bins=50)
# Optional adding of fill colors
pl + scale_fill_manual(values = c('red','white')) + theme_bw()



#6 Create a Histogram of citric.acid from the wine data. Color by red and white wines.

head(wine)

pl <- ggplot(wine,aes(x=citric.acid)) + 
  geom_histogram(aes(fill=label),color='black',bins=50)
pl + scale_fill_manual( aes(fill=label) ,values = c('red','white')) + theme_bw()



#7 Create a Histogram of alcohol from the wine data. Color by red and white wines.

head(wine)

pl <- ggplot(wine,aes(x=alcohol)) + 
  geom_histogram(aes(fill=label),color='black',bins=50)
pl + scale_fill_manual( aes(fill=label) ,values = c('red','white')) + theme_bw()


#8 Create a scatterplot of residual.sugar versus citric.acid, 
#color by red and white wine.


head(wine)

pl <- ggplot(wine,aes(x=citric.acid , y = residual.sugar, color = label)) + 
  geom_point(aes(fill=label) ,bins=50, alpha = 0.1)  

pl + scale_color_manual(values=c('red','white')) +  theme(panel.background = element_rect(fill = 'dark grey'), panel.grid.major = element_line(colour = "grey20"), panel.grid.minor = element_line(colour = "grey50"))



#9 Create a scatterplot of volatile.acidity versus residual.sugar, color by red and white wine.


head(wine)

pl <- ggplot(wine,aes(x=volatile.acidity , y = residual.sugar, color = label)) + 
  geom_point(aes(fill=label) ,bins=50, alpha = 0.1)  

pl + scale_color_manual(values=c('red','white')) +  theme(panel.background = element_rect(fill = 'dark grey'), panel.grid.major = element_line(colour = "grey20"), panel.grid.minor = element_line(colour = "grey50"))




#10 Grab the wine data without the column 'label' and call it clus.data, 
#and check the head of clus.data

head(wine)
tail(wine)


#clus.data <- data.frame(wine$fixed.acidity,wine$volatile.acidity,wine$citric.acid,wine$residual.sugar,wine$chlorides,wine$free.sulfur.dioxide,wine$total.sulfur.dioxide,wine$density,wine$pH,wine$sulphates,wine$sulphates,wine$alcohol,wine$quality)

#head(clus.data) 

clus.data <- wine[,1:12]
head(clus.data)


#11 Call the kmeans function on clus.data and assign the results to wine.cluster.

#irisCluster <- kmeans(clus.data,  3, nstart=20)

wine.cluster <- kmeans(clus.data,2, nstart = 20)

#12 Print out the wine.cluster Cluster Means and explore the information.
wine.cluster


#13 Use the table() function to compare your cluster results to the real results. 
#Which is easier to correctly group, red or white wines?
table(wine.cluster$cluster, wine$label)
library("cluster")
table( wine$label, wine.cluster$cluster)

clusplot(wine, wine.cluster$cluster, color=T, shade = T, labels = 0, lines=0)

head(iris)
iris_1 <- iris[, -5]
head(iris_1)
#red wine is easier to group 




# Installing Packages
install.packages("ClusterR")
install.packages("cluster")

# Loading package
library(ClusterR)
library(cluster)

# Removing initial label of 
# Species from original dataset
iris_1 <- iris[, -5]
head(iris_1)

# Fitting K-Means clustering Model 
# to training dataset
set.seed(240) # Setting seed
kmeans.re <- kmeans(iris_1, centers = 3, nstart = 20)
kmeans.re

# Cluster identification for 
# each observation
kmeans.re$cluster

# Confusion Matrix
cm <- table(iris$Species, kmeans.re$cluster)
cm

# Model Evaluation and visualization
plot(iris_1[c("Sepal.Length", "Sepal.Width")])
plot(iris_1[c("Sepal.Length", "Sepal.Width")], 
     col = kmeans.re$cluster)
plot(iris_1[c("Sepal.Length", "Sepal.Width")], 
     col = kmeans.re$cluster, 
     main = "K-means with 3 clusters")

## Plotiing cluster centers
kmeans.re$centers
kmeans.re$centers[, c("Sepal.Length", "Sepal.Width")]

# cex is font size, pch is symbol
points(kmeans.re$centers[, c("Sepal.Length", "Sepal.Width")], 
       col = 1:3, pch = 8, cex = 3) 

## Visualizing clusters
y_kmeans <- kmeans.re$cluster
clusplot(iris_1[, c("Sepal.Length", "Sepal.Width")],
         y_kmeans,
         lines = 0,
         shade = TRUE,
         color = TRUE,
         labels = 2,
         plotchar = FALSE,
         span = TRUE,
         main = paste("Cluster iris"),
         xlab = 'Sepal.Length',
         ylab = 'Sepal.Width')
