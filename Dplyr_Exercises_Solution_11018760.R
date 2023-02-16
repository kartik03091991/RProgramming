
library(dplyr)

# 2 
df <- mtcars
#pipe operator  %>%

df %>% filter(mpg>20) %>% sample_n(size = 10) %>% arrange(desc(mpg))




#3. Return rows of cars that have an mpg value greater than 20 and 6 cylinders. 

df %>% filter(mpg>20)  %>% filter(cyl == 6)




#4 . Reorder the Data Frame by cyl first, then by descending wt.




df %>% arrange(cyl,desc(wt))  






#5 Select the columns mpg and hp

select(df,mpg,hp)

#select(df,mpg,hp) %>% filter(mpg>20) 


#6 Select the distinct values of the gear column.

distinct(select(df,gear))


#distinct(select(df,gear)) %>% filter(gear == 3)

#7 Create a new column called "Performance" which is calculated by hp divided by wt.


performance <- mutate(df, performance = hp/wt)

performance


#8 Find the mean mpg value using dplyr.



df$mpg -> a 

sum(a)

nrow(df)

sum(a)/nrow(df)

#sum(df$mpg)/nrow(df)  

#9 Use pipe operators to get the mean hp value for cars with 6 cylinders.


df %>% filter(cyl == 6) -> b

df %>% filter(cyl == 6) %>% nrow -> c

sum(b$hp) -> d

d/c


