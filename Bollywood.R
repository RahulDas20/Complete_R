#loading library
library(tidyverse)
library(ggplot2)
library(dplyr)
library(broom)


##reading the data from file##
data <- read.csv("Data for repository.csv")
head(data)


##CONVERSION OF DATA AS FACTOR##
data$Release_Period <- as.factor(data$Release_Period)
data$Whether_Remake <- as.factor(data$Whether_Remake)
data$Whether_Franchise <- as.factor(data$Whether_Franchise)
data$Genre <- as.factor(data$Genre)
data$New_Actor <- as.factor(data$New_Actor)
data$New_Director <- as.factor(data$New_Director)
data$New_Music_Director <- as.factor(data$New_Music_Director)
data$Revenue.INR. <- as.numeric(data$Revenue.INR.)
data$Number_of_Screens <- as.numeric(data$Number_of_Screens)
str(data)


##create a new data column called profit

data <- data %>%
  mutate(profit = data$Revenue.INR. - data$Budget.INR.)

str(data)

#exploratory data analysis
#creating histogram to see of the data is normally distributed

hist(data$Number_of_Screens)
hist(data$profit)
hist(data$Revenue.INR.)

pairs(~ Number_of_Screens +
        profit + 
        Revenue.INR. +
        Budget.INR.,data = data)



fit <- lm(profit ~ Number_of_Screens + Budget.INR.,data = data)

tidy(fit)

data$profit
data$Predicted_rev
data$Predicted_prof <- predict(fit, newdata = data)

data %>%
  ggplot(aes(profit,Predicted_prof, Genre)) +
  geom_point(aes(col = Genre)) +
  geom_abline()
