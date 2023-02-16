#1 Read in the adult_sal.csv file and set it to a data frame called adult, 
#and Check the head of adult.

adult <- read.csv("C:/Users/User/Downloads/adult_sal (1).csv")

head(adult)

#2 You should notice the index has been repeated. Drop this column.

library(dplyr) 
adult <- select(adult,-X)
head(adult)

#3 Check the head,str, and summary of the data now.

head(adult)

str(head)

summary(adult)

#4 Use table() to check out the frequency of the type_employer column.

table()

table(adult$type_employer)

#5 How many Null values are there for type_employer? 
#What are the two smallest groups?


#1836 null values

#never worked and without pay



#6 Combine these two smallest groups into a single group called "Unemployed". 
#There are lots of ways to do this, so feel free to get creative. Hint: 
#It may be helpful to convert these objects into character data types (as.character() 
#and then use sapply with a custom function)




adult$type_employer[adult$type_employer == 'Never-worked'] <- 'Unemployed'
adult$type_employer[adult$type_employer == 'Without-pay'] <- 'Unemployed'


#7 What other columns are suitable for combining? 
#Combine State and Local gov jobs into a category called SL-gov and combine 
#self-employed jobs into a category called self-emp


adult$type_employer[adult$type_employer == 'State-gov'] <- 'SL-gov'
adult$type_employer[adult$type_employer == 'Local-gov'] <- 'SL-gov'


adult$type_employer[adult$type_employer == 'Self-emp-inc'] <- 'self-emp'
adult$type_employer[adult$type_employer == 'Self-emp-not-inc'] <- 'self-emp'


table(adult$type_employer)



#8 Use table() to look at the marital column.

table(adult$marital)



#9 Reduce this to three groups:
#Married
#Not-Married
#Never-Married


adult$marital[adult$marital == 'Married-civ-spouse'] <- 'Married'
adult$marital[adult$marital == 'Married-AF-spouse'] <- 'Married'
adult$marital[adult$marital == 'Married-spouse-absent'] <- 'Married'

adult$marital[adult$marital == 'Divorced'] <- 'Not-Married'
adult$marital[adult$marital == 'Separated'] <- 'Not-Married'
adult$marital[adult$marital == 'Widowed'] <- 'Not-Married'

table(adult$marital)


#10 Check the country column using table().

table(adult$country)



#11 Group these countries together however you see fit. 
#You have flexibility here because there is no right/wrong way to do this, 
#possibly group by continents. 
#You should be able to reduce the number of groups here significantly though.






Asia <- c('China','Hong','India','Iran','Cambodia','Japan', 'Laos', 'Philippines' ,'Vietnam' ,'Taiwan', 'Thailand') 

North.America <- c('Canada','United-States','Puerto-Rico' ) 

Europe <- c('England' ,'France', 'Germany' ,'Greece','Holand-Netherlands','Hungary', 'Ireland','Italy','Poland','Portugal','Scotland','Yugoslavia') 

Latin.and.South.America <- c('Columbia','Cuba','Dominican-Republic','Ecuador', 'El-Salvador','Guatemala','Haiti','Honduras', 'Mexico','Nicaragua','Outlying-US(Guam-USVI-etc)','Peru',            'Jamaica','Trinadad&Tobago')

Other <- c('South') 




group_country <- function(ctry){ 
  if (ctry %in% Asia){
    return('Asia')     }
  else if (ctry %in% North.America){ 
    return('North.America')     }
  else if (ctry %in% Europe){
    return('Europe')     }
  else if (ctry %in% Latin.and.South.America){ 
    return('Latin.and.South.America')     }
  else{ 
    return('Other')           } 
}






adult$country <- sapply( adult$country, group_country )

#12 Use table() to confirm the groupings.

table(adult$country)


#13 Check the str() of adult again. 
#Make sure any of the columns we changed have factor levels with factor().

str(adult2)

adult2$type_employer <- as.factor(adult2$type_employer)
adult2$country <- as.factor(adult2$country)
adult2$marital <- as.factor(adult2$marital)

str(adult2)



#14 Convert any cell with a '?' or a ' ?' value to a NA value. Hint: is.na() may be useful here or 
#you can also use brackets with a conditional statement. 
#Refer to the solutions if you can't figure this step out.

table(adult$type_employer)
table(adult$country)
table(adult$marital)

library(Amelia) 

group_na <- function(gna){ 
  if ((gna == '?') ){
    return(NA)     }
  else{
      return(gna)
    }
}


levels(adult2$type_employer)

adult2$type_employer <- sapply( adult2$type_employer, group_na )
 




#15 Using table() on a column with NA values should now not display 
#those NA values, instead you'll just see 0 for ?. 
#Optional: Refactor these columns (may take awhile). For example

table(adult2$type_employer)


#16 Play around with the missmap function from the Amelia package. 
#Can you figure out what its doing and how to use it?


library(Amelia)

missmap(adult2)



#17 You should have noticed that using missmap(adult) is bascially a heatmap pointing out 
#missing values (NA). This gives you a quick glance at how much data is missing, 
#in this case, not a whole lot (relatively speaking). You probably also noticed that 
#there is a bunch of y labels, 
#get rid of them by running the command below. What is col=c('yellow','black') doing?

missmap(adult2,y.at=c(1),y.labels = c(''),col=c('yellow','black'))



#18 Use na.omit() to omit NA data from the adult data frame. Note, it really depends 
#on the situation and your data to judge 
#whether or not this is a good decision. You shouldn't always just drop NA values

adult3 <- adult2 


na.omit(adult2)


#19 Use missmap() to check that all the NA values were in fact dropped.

missmap(na.omit(adult2))

missmap(na.omit(adult2),y.at=c(1),y.labels = c(''),col=c('yellow','black'))


#20 Check the str() of the data.

str(adult2)


#21 Use ggplot2 to create a histogram of ages, colored by income.

head(adult2)

library(ggplot2)

pl <- ggplot(adult2,aes(x=age )) + geom_histogram(aes(fill=income),color = 'black', bins  = 60)
pl + scale_fill_manual(values = c('red','cyan')) 





#22 Plot a histogram of hours worked per week.
#`stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

head(adult2)



pl <- ggplot(adult2,aes(x=hr_per_week )) + geom_histogram( bins  = 30)
pl 


#23 Rename the country column to region column to better reflect the factor levels.



names(adult2)[names(adult2) == "country"] <- "region"


head(adult2)


#24 Create a barplot of region with the fill color defined by income class. 
#Optional: Figure out how rotate the x axis text for readability.

str(adult2)



pl <- ggplot(adult2,aes(x=region )) + geom_bar(aes(fill=income),color = 'black', bins  = 60)
pl + scale_fill_manual(values = c('red','cyan')) 




#25 Take a quick look at the head() of adult to make sure we have a 
#good overview before going into building the model!

head(adult2)



#26 Train Test Split:
#Split the data into a train and test set using the caTools library as done in previous lectures. 
#Reference previous solutions notebooks if you need a refresher.


library(caTools)
set.seed(102)



split = sample.split(adult2, SplitRatio = 0.70)

final.train = subset(adult2, split == TRUE)
final.test = subset(adult2, split == FALSE)


#27 Explore the glm() function with help(glm). Read through the documentation.

help(glm)


#28 Use all the features to train a glm() model on the training data set, 
#pass the argument family=binomial(logit) into the glm function.



str(adult2)

final.train$income <- factor(final.train$income)

str(final.train)

final.log.model <- glm(formula=income ~ . , family = binomial(link='logit'),data = final.train)


#29 If you get a warning, this just means that the model may have guessed the probability
#of a class with a 0% or 100% chance of occuring.
#Check the model summary
summary(final.log.model)

#age+type_employer+fnlwgt+education+education_num+marital+occupation+relationship+race+sex+capital_gain+capital_loss+hr_per_week+region+income


#30 Use new.model <- step(your.model.name) to use the step() function to create a new model.

new.final.log.model <- step(final.log.model)

#31 You should get a bunch of messages informing you of the process. 
#Check the new.model by using summary()


summary(new.final.log.model)

#32 Create a confusion matrix using the predict function with type='response' 
#as an argument inside of that function.


final.test$income <- factor(final.test$income)

fitted.probabilities <- predict(final.log.model,newdata=final.test,type='response')
table_mat <- table(final.test$income, fitted.probabilities > 0.5)

table_mat


#33 What was the accuracy of our model?
#accuracy = TruePositives + TrueNegatives/ (TruePositives + FalsePositives + TrueNegatives + FalseNegatives)


accuracy_Test <- sum(diag(table_mat)) / sum(table_mat)
accuracy_Test


#34 Calculate other measures of performance like, recall or precision.
#Recall = TruePositives / (TruePositives + FalseNegatives)
#Precision = TruePositives / (TruePositives + FalsePositives)


precision <- function(matrix) {
  # True positive
  tp <- matrix[2, 2]
  # false positive
  fp <- matrix[1, 2]
  return (tp / (tp + fp))
}



recall <- function(matrix) {
  # true positive
  tp <- matrix[2, 2]# false positive
  fn <- matrix[2, 1]
  return (tp / (tp + fn))
}



prec <- precision(table_mat)
prec
rec <- recall(table_mat)
rec


