#1 Use the ISLR libary to get the iris data set. Check the head of the iris Data Frame.

library(ISLR)

head(iris)

#2 Use scale() to standardize the feature columns of the iris dataset. 
#Set this standardized version of the data as a new variable.


#standardized.Caravan <- scale(Caravan[,-86])
standardized.iris <- scale(iris[,-5])


#3 Check that the scaling worked by checking the variance of one of the new columns.

summary(standardized.iris)

var(standardized.iris[,1])

var(standardized.iris[,2])



#4 Join the standardized data with the response/target/label 
#column (the column with the species names.


head(standardized.iris)

stand_new <- cbind(standardized.iris,iris[5])

head(stand_new)



#5 Use the caTools library to split your standardized data into train and test sets.
#Use a 70/30 split.

library(caTools)


sample <- sample.split(stand_new$Species, SplitRatio = .70)
train <- subset(stand_new, sample == TRUE)
test <- subset(stand_new, sample == FALSE)



#6 Build KNN model Call the class library: library(class)

library(class)


#7 Use the knn function to predict Species of the test set. Use k=1


predicted.species <- knn(train[1:4],test[1:4],train$Species,k=1)
predicted.species

#8 What was your misclassification rate?


#misclass.error <- mean(test.purchase != predicted.purchase)

misclass.error <- mean(test$Species != predicted.species)



#9 Create a plot of the error (misclassification) rate for k values ranging from 1 to 10.


predicted.species <- NULL 
error.rate <- NULL 

for(i in 1:10){ 
  set.seed(101) 
  predicted.species <- knn(train[1:4],test[1:4],train$Species,k=i) 
  error.rate[i] <- mean(test$Species != predicted.species) 
}


plot(1:10, error.rate, col='red', type = 'b', ylim = c(0,0.4))


