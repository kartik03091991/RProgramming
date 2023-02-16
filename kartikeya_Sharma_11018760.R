#1. Read in the Advertisements.csv file, and save the columns 3 to 5 to a 
#data frame called dataset, and continue the following steps on dataset.


df <- read.csv("C:/Users/User/Downloads/Advertisements.csv")
head(df)

dataset <- df[,3:5]

head(dataset)


#2 In each step make sure that you are installing the necessary libraries.

library(caTools)

#3 set.seed(123)

set.seed(123)



#4 Make a test set out of our training set, retrain on the smaller version of our 
#training set and check it against the test subset. (Hint set the SplitRatio = 0.75) 

library(caTools)
sample <- sample.split(dataset, SplitRatio = 0.75)

train <- subset(dataset, sample==TRUE)
#30% of data -> test
test <- subset(dataset, sample==FALSE)


#5 Apply feature scaling (using scale) on 
#Age and EstimatedSalary columns of your training and testing sets.

scale(train$Age, center = TRUE, scale = TRUE)
scale(train$EstimatedSalary, center = TRUE, scale = TRUE)
scale(train$Purchased, center = TRUE, scale = TRUE)


train$Age  <- scale(train$Age, center = TRUE, scale = TRUE)
train$EstimatedSalary <- scale(train$EstimatedSalary, center = TRUE, scale = TRUE)
train$Purchased <- scale(train$Purchased, center = TRUE, scale = TRUE)


test$Age  <- scale(test$Age, center = TRUE, scale = TRUE)
test$EstimatedSalary <- scale(test$EstimatedSalary, center = TRUE, scale = TRUE)
test$Purchased <- scale(test$Purchased, center = TRUE, scale = TRUE)



#6 Use all the features to train a glm() model on the training data set to predict 
#Purchased based on all 
#other features, pass the argument family=binomial into the glm function.

#temp.model <- lm(count ~ temp, data= train)

train$Purchased <- factor(train$Purchased)

final.log.model <- glm(formula=Purchased ~ . , family = binomial(link='logit'),data = train)
summary(final.log.model)

#train1 <- train

#train$Purchased <- abs(train$Purchased)

#final.log.model <- glm(formula=Survived ~ . , family = binomial(link='logit'),data = final.train)
#summary(final.log.model)


#7 Predict the test set result (as we are trying to predict "Purchased", make sure this column is 
#removed from the test set in your prediction). Save your result in prop_prep

head(test)

test2 <- test[,-3]

head(test2)

prop_prep <- predict(final.log.model,newdata=test2,type='response')

prop_prep


#8 In prop_prep, replace the probabilities greater than 0.5 to 1, otherwise to 0

fitted.results <- ifelse(prop_prep > 0.5,1,0)


#9 Make the confusion Matrix.

table(test$Purchased, fitted.results > 0.5)

#10  What was the accuracy of our model? 

summary(final.log.model)


misClasificError <- mean(fitted.results != test$Purchased)
#It is near 1, so it is a good accuracy
print(paste('Accuracy: ',1-misClasificError))

#0 is the accuracy



##2nd question 

#1.Read in the Advertisements.csv file, and save the columns 3 to 5 to a data frame called dataset, 
#and continue the following steps on dataset

df <- read.csv("C:/Users/User/Downloads/Advertisements.csv")
head(df)

dataset <- df[,3:5]

head(dataset)


#2 Convert Purchased column to a factor.

dataset$Purchased <- factor(dataset$Purchased)

str(dataset)

#3 set.seed(123)

set.seed(123)

#4 Make a test set out of our training set, 
#retrain on the smaller version of our training set and check 
#it against the test subset.  (Hint set the SplitRatio = 0.75)

library(caTools)
sample <- sample.split(dataset$Purchased, SplitRatio = 0.75)

train <- subset(dataset, sample==TRUE)
#30% of data -> test
test <- subset(dataset, sample==FALSE)



#5 Use the rpart library to build a decision tree to predict Purchased based on all other features.

library(ISLR)
library(tidyverse)
library(caTools)
library(rpart)
library(rpart.plot)

tree_model <- rpart(Purchased ~ . , method='class', data= train)
summary(tree_model)

#6 Use predict() to predict the Purchased on the test data, and make the Confusion Matrix.

predicted_values <- predict(tree_model, test[,-which(colnames(test)=='Purchased')])

head(predicted_values)



#7 Visualize or plot your tree. 
#First using plot and text function to add text to it, and then using prp.

prp(tree_model)
prp(tree_model,main="Tree Model")



#8 Apply feature scaling (using scale) on all columns 
#of your training and testing sets except the last column.


train$Age  <- scale(train$Age, center = TRUE, scale = TRUE)
train$EstimatedSalary <- scale(train$EstimatedSalary, center = TRUE, scale = TRUE)



test$Age  <- scale(test$Age, center = TRUE, scale = TRUE)
test$EstimatedSalary <- scale(test$EstimatedSalary, center = TRUE, scale = TRUE)


#9 Use KNN on the training set and predict the test set with k=5.
library(class)
library(caTools)
predicted.purchased <- knn(train[1:2],test[1:2],train$Purchased,k=5)
predicted.purchased


#10 Make the Confusion Matrix.

table(test$Purchased,predicted.purchased)



#11. Now use the svm() function to train a model on your training set. Add these arguments 
#inside your svm model (type='C-classification', kernel='linear').
library(e1071)

tuned.svm <- svm(formula - train$Purchased~., data = train, kernel = "linear")



#12 Predict the Test set results, and make the Confusion Matrix





# question3 

#1 Install and load the library tidyverse, use the dataset mpg.

library(tidyverse)

head(mpg)


#2 Make a scatterplot of class vs drv.

pl <- ggplot(mpg,aes(x=drv , y =  class)) + 
  geom_point()  
pl


#3 Create the following plots:

library(ggplot2)
#a
pl <- ggplot(mpg,aes(x = displ , y = hwy)) + 
  geom_point( aes(fill = cty,color = cty ), size=2,alpha=0.6)  
pl


#b
pl <- ggplot(mpg,aes(x = manufacturer)) + 
  geom_bar( aes(fill = class))  
pl



# question 4

#1 Create a Matrix with 3 rows and 2 columns. Fill it with numbers from 1 to 6.



x <- c(1:6)
x

#mat <- matrix(x , nrow = 3, byrow = TRUE )

mat <- matrix(x , nrow = 3)

mat



#2 Turn the previous Matrix in to a data frame.


dataframe1 <- as.data.frame(mat)

dataframe1



#3 Create a 3 element numeric vector, a 3 element character vector, 
#and a 3 element factor vector (hint, use as.factor() on 3 element character vector).



x <- c(1, 5, 4)
typeof(x)

y <- c('A','B','C')
typeof(x)

z <- as.factor(y)
typeof(z)


#4 For part3, combine the vectors into a matrix, and then combine the vectors in to a data frame.

mat1 <- cbind(x, y,z)
mat1


my_data <- data.frame(x,y,z)


#5 For the data frame in part4, rename each column.

colnames(my_data)[1] ="c1"
colnames(my_data)[2] ="c2"
colnames(my_data)[3] ="c3"
my_data



#6 Create a 5 element numeric vector, and 3*2 numeric matrix. Add the third element of the 
#vector to the element in the second row and the first column of the Matrix.


x <- c(1:5)
x








