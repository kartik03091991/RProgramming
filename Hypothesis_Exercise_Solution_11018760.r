#1 Load Recycling.csv file.
recycle <- read.csv('C:/Users/User/Downloads/Recycling.csv')
recycle 

#2 Save the data in 'Recycle.Weight' column in a column called 'x'
recycle$x <- recycle$Recycle.Weight
recycle$Recycle.Weight
recycle$x

#3 Now calculate the mean and sd of 'x'.
mean(recycle$x)
sd(recycle$x)

#4 Calculate the t-test of 'x' against a null hypothesis by considering that population mean (mu) is 8.4
t.test(recycle,mu=8.4)

#5 Check if the null hypothesis is true, or the null hypothesis is rejected.
#p-value = 2.2e-16  which is less than significant level (0.05) ,That's why we  reject null Hypothesis

#6 Exercie #2 Load the taxis.csv file could not find the taxis file