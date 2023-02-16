#1. Read in bikeshare.csv file and set it to a dataframe called bike, 
#and Check the head of df.


bike <- read.csv("C:/Users/User/Downloads/bikeshare.csv")

head(bike)


#2 Create a scatter plot of count vs temp. Set a good alpha value.

library(ggplot2)


pl <- ggplot(bike,aes(x=temp , y =  count)) + 
  geom_point( aes(fill = temp,color = temp ), alpha = 0.1)  
pl


#3 Plot count versus datetime as a scatterplot with a color gradient 
#based on temperature. You'll need to convert the datetime column 
#into POSIXct before plotting.
bike$datetime <- as.Date(bike$datetime)

bike

pl <- ggplot(bike,aes(x=datetime , y =  count , color = temp)) + 
  geom_point() 
pl + scale_color_gradient(low="cyan", high="dark orange") 




#sp3+scale_color_gradientn(colours = rainbow(5))


#df$DateTime <- as.POSIXct(paste(df$Date, df$Time), format="%Y%m%d %H%M%S")


#bike$datetime <- as.POSIXct(paste(df$datetime), format="%Y%m%d %H%M%S")


# 4 What is the correlation between temp and count?



cor(bike$count, bike$temp)
cor(bike$temp, bike$count)

x <- cor(bike$count, bike$temp)
y <- cor(bike$temp, bike$count)



table(x, y)

#5 Let's explore the season data. Create a boxplot, with the y axis indicating count
#and the x axis begin a box for each season.


#a <- factor(df,season)


#help(factor)

#distinct(select)

p <- ggplot(bike, aes(x=factor(season), y=count)) + 
  geom_boxplot(aes(color = factor(season)))#,color = c('red','blue','orange','purple'))
p


#6 Create an "hour" column that takes the hour from the datetime column. 
#You'll probably need to apply some function to the 
#entire datetime column and reassign it. Hint:

bike <- read.csv("C:/Users/User/Downloads/bikeshare.csv")

str(bike)

t.str <- strptime(bike$datetime, "%Y-%m-%d %H:%M:%S")
hour <- as.numeric(format(t.str, "%H"))

bike <- cbind(bike, hour)

head(bike)


#7 Now create a scatterplot of count versus hour, with color scale based on temp. 
#Only use bike data where workingday==1.

#Optional Additions:
#Use the additional layer: scale_color_gradientn(colors=c('color1',color2,etc..)) 
#where the colors argument is a vector gradient of colors you choose, not just high and low.
#Use position=position_jitter(w=1, h=0) inside of geom_point() and check out what it does.

library(dplyr)

df2 <- filter(bike, workingday == 1)
  
#a <- filter(df, mpg>20)


pl <- ggplot(df2,aes(x=hour , y =  count , color = temp)) + 
  geom_point(position=position_jitter(w=1, h=0) ) 
pl + scale_color_gradientn(colours = rainbow(7))



#8 Now create the same plot for non working days: 

df3 <- filter(bike, workingday == 0)

pl <- ggplot(df3,aes(x=hour , y =  count , color = temp)) + 
  geom_point(position=position_jitter(w=1, h=0) ) 
pl + scale_color_gradientn(colours = rainbow(5))



#9 Use lm() to build a model that predicts count based solely on the temp feature, 
#name it temp.model
library(caTools)
sample <- sample.split(bike$count, SplitRatio = 0.7)

train <- subset(bike, sample==TRUE)
#30% of data -> test
test <- subset(bike, sample==FALSE)

temp.model <- lm(count ~ temp, data= train)

#temp.model <- lm(count ~ temp ,bike)

#plot(temp.model)

#10 Get the summary of the temp.mode.


summary(temp.model)

library(caTools)

#11 How many bike rentals would we predict if the temperature was 
#25 degrees Celsius? Calculate this two ways:

count.predeict <- predict(temp.model, test) 
count.predeict





#12 Use sapply() and as.numeric to change the hour column to a column of numeric values.

head(bike)

to_numeric <- function(x){
   as.numeric(x)
}

bike$hour <- sapply(bike$hour, to_numeric)
bike$hour 

str(bike)



#13 Finally build a model that attempts to predict count based off of the following features. 
#Figure out if theres a way to not have to pass/write all these variables into the lm() function. 
#Hint: StackOverflow or Google may be quicker than the documentation.
#season
#holiday
#workingday
#weather
#temp
#humidity
#windspeed
#hour (factor)


sample <- sample.split(bike$count, SplitRatio = 0.7)

train <- subset(bike, sample==TRUE)
#30% of data -> test
test <- subset(bike, sample==FALSE)

temp.model <- lm(count ~ season+holiday+workingday+weather+temp+humidity+windspeed+factor(hour), data= train)

#14 Get the summary of the model.

summary(temp.model)







