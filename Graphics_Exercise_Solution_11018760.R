library(ggplot2)
library(ggplot2movies)

library(ggplot2)
library(ggthemes) 
head(mpg)


mpg

binwidth = 1

mtcars

hwy



pl <- ggplot(mtcars, aes(x=mpg)) 
pl + geom_histogram(color='red',fill='pink')


#1

pl <- ggplot(mpg, aes(x=hwy)) 
pl + geom_histogram(fill='red')



# 2 Barplot of car counts per manufacturer with color fill defined by cyl count.

mtcars

pl <- ggplot(mpg, aes(x=manufacturer)) 
pl + geom_histogram()

 




# 5 Create a scatterplot of volume versus sales.
#Afterwards play around with alpha and color arguments to clarify information.

txhousing


pl <- ggplot(txhousing, aes(x = sales , y = volume))
pl +  geom_point(color = 'blue')


# 6 Add a smooth fit line to the scatterplot from above. 
#Hint: You may need to look up geom_smooth()

txhousing


pl <- ggplot(txhousing, aes(x = sales , y = volume))
pl +  geom_point(color = 'blue') + geom_smooth(color = 'red')



