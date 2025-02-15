#look at the US murders data
library(dslabs)
data("murders")
library(tidyverse)
library(ggplot2)
install.packages("ggthemes")
library(ggthemes)
library(ggrepel)

str(murders)

#which state have the largest and the smallest population?
#how large is a typical state?
#what is the relation between population and murder num?

#The great value of a picture is when it forces us to notice what we never expected to see
#how to use aesthetics mappings in ggplot
murders %>% 
  ggplot() +
  geom_point(aes(x = population/10^6, y = total)) +
  geom_text(aes(population/10^6, total, label = abb))


#tinkering with arguments
p <- ggplot(data = murders)


p + geom_point(aes(population/10^6, total), size = 3) +
  geom_text(aes(population/10^6, total, label = abb), nudge_x = 1.5)


#we can also write this code in the below format
p <- murders %>% ggplot(aes(population/10^6, total, label= abb))

p + geom_point(size = 3) +
  geom_text(nudge_x = 0.05) +
  scale_x_log10() +
  scale_y_log10()


#How to add Labels and titles to the graph
murders %>%
  ggplot(aes(population/10^6, total, label = abb)) +
  geom_point(size = 1.5) +
  geom_text(nudge_x = 0.05) +
  scale_x_log10() +
  scale_y_log10() +
  xlab("Population in millions (log scale)") +
  ylab("Total number of murders (log scale)") +
  ggtitle("US Gun Murders in 2010")



## how to add different catagories as color
murders %>%
  ggplot(aes(population/10^6, total, label = abb)) +
  geom_point(aes(col = region),size = 1.5) +
  geom_text(nudge_x = 0.05) +
  scale_x_log10() +
  scale_y_log10() +
  xlab("Population in millions (log scale)") +
  ylab("Total number of murders (log scale)") +
  ggtitle("US Gun Murders in 2010")


##Annotation, shape and adjustment
#we want to draw a line with avarage murder rate

r <- murders %>%
  summarise(rate = sum(total)/ sum(population) * 10^6) %>%
  pull(rate)

## how to add a line to the graph
murders %>%
  ggplot(aes(population/10^6, total, label = abb)) +
  geom_point(aes(col = region),size = 1.5) +
  geom_text(nudge_x = 0.05) +
  scale_x_log10() +
  scale_y_log10() +
  geom_abline(intercept = log10(r), color="red")+
  xlab("Population in millions (log scale)") +
  ylab("Total number of murders (log scale)") +
  ggtitle("US Gun Murders in 2010") +
  theme_economist()

p <- p + geom_abline(intercept = log10(r), lty = 2, color = "darkgrey") +
  geom_point(aes(col=region), size = 3)

p <- p + scale_color_discrete(name = "Region")


murders %>%
  ggplot(aes(population/10^6, total, label = abb)) +
  geom_abline(intercept = log10(r), color = "red", ity = 2) +
  geom_point(aes(col = region),size = 2) +
  geom_text_repel() +
  scale_x_log10() +
  scale_y_log10() +
  xlab("Population in Million(log scale)") +
  ylab("Total Number of Murders(log scale)") +
  ggtitle("US Gun Murders in 2010") +
  scale_color_discrete(name = "Region") +
  theme_economist()


#Quick plots with qpolt
data("murders")
x <- log10(murders$population)
y <- murders$total

qplot(x,y)


#How can we make a grid of plots
library(gridExtra)
p1 <- qplot(x)
p2 <- qplot(x,y)
grid.arrange(p1,p2, nrow =2)


##Exercises
library(dplyr)
library(ggplot2)
library(dslabs)
data("heights")
data("murders")


p <- ggplot(murders)

q <- ggplot(heights)

murders %>% ggplot(aes(population, total)) +
  geom_point()

murders %>% ggplot(aes(population, total, label = abb)) +
  geom_point(aes(col = region), size = 1.5) +
  geom_text_repel() +
  scale_x_log10() +
  scale_y_log10() +
  theme_excel_new()

##.........................................##
##VISUALIZING DATA DISTRIBUTION##
#...........................................##

#ordinal data is variables belonging to small number of different groups, with each groups having many members
##CASE STUDY
##Student Heights

library(tidyverse)
library(dslabs)
data(heights)

str(heights)

heights %>%
  ggplot(aes(height)) +
  geom_histogram(binwidth = 1,color = "white", fill = "orange") +
  facet_grid(sex ~ .)

hist(heights$height)


#THE NORMAL DISTRIBUTION
#if we have a vector X
#then the avarage of x will be mean(x)
#and the standard deviation will be sd(x)

#if the data is normally distributed then we can measure the data in standard units
#z = (x - m)/s with m is the mean and s is the standard deviation

##........................###
## QUANTILE QUANTILE PLOTS#
##.........................###


index <- heights$sex == "Male"
x <- heights$height[index]

x

qnorm(0.975, mean = 5, sd = 2)

mean(x <= 69.5)

## Let's construct a qq plot with R code
p <- seq(0.05, 0.95, 0.05)

sample_quantiles <- quantile(x, p)

theoretical_quantiles <- qnorm(p, mean = mean(x), sd = sd(x))

theoretical_quantiles

qplot(theoretical_quantiles,sample_quantiles) +
  geom_abline()


z <- scale(x)

sample_quantiles <- quantile(z,p)
theoretical_quantiles <- qnorm(p, mean = mean(x), sd = sd(x))

qplot(theoretical_quantiles,sample_quantiles) +
  geom_abline()

heights %>% filter(sex == "Male") %>%
  ggplot(aes(sample = scale(height))) +
  geom_qq() +
  geom_abline()



## We are going to do the qq ploting again
p <- seq(0.05, 0.95, 0.05)


#to obtain sample quantile from the data, we can use the quantile function
sample_quantiles <- quantile(x,p)

#now let's define theoretical quantile
theoretical_quantiles <- qnorm(p, mean = mean(x), sd = sd(x))
sample_quantiles

#now plot these points
qplot(theoretical_quantiles, sample_quantiles) + geom_abline()



## now let's do these with z score
z <- scale(x)


#lets define sample quantile from z scores
sample_quantiles <- quantile(z, p)
sample_quantiles


#lets determine the theoretical quantile
theoretical_quantiles <- qnorm(p, mean = mean(z), sd = sd(z))

#let's plot these data
qplot(theoretical_quantiles,sample_quantiles) + geom_abline()



#now we understand how the qq plots work in the normal distribution
#we can inspect any data with these qqplot function


#the qq aesthetics should have the sample parameter in the ggplot aesthetics

heights %>% filter(sex == "Male") %>%
  ggplot(aes(sample = scale(height))) +
  geom_qq() +
  geom_abline()


###EXERCISES####
#Define variables containing heights

library(dslabs)
library(tidyverse)
male <- heights$height[heights$sex == "Male"]
female <- heights$height[heights$sex == "Female"]

p <- seq(.10,.90,0.10)

#let's create the male percentile
male_percentile <- quantile(male, p)
male_percentile

#let's create the female percentile
female_percentile <- quantile(female, p)

#lets create the data frame
heights_data <- data.frame(male_percentile,female_percentile)
heights_data



library(dslabs)
data("heights")

x <- heights$height[heights$sex == "Male"]

mean(x > 69 & x <= 72)

avg <- mean(x)
sd <- sd(x)

prop2 <- pnorm(72, avg, sd)
prop2


prop1 <- pnorm(69,avg, sd)
prop1

prop2 - prop1


#sometimes we have a table which we want to show as a boxplot
data("murders")

tab <- murders %>%
  count(region) %>%
  mutate(proportion = n/sum(n))


#we use stat indentity when we want to show the actual data in the barplot, not the proportion
tab %>% ggplot(aes(region,proportion)) + geom_bar(stat = "identity")
#we no longer want geom_bar to count but rather plot the height data provided by the proportion variable

tab %>% ggplot(aes(region,proportion)) + 
  geom_bar(stat = "identity")



###HISTOGRAMS###
heights %>%
  filter(sex == "Female") %>%
  ggplot(aes(height)) +
  geom_histogram(binwidth = 1, color = "black", fill = "orange") +
  xlab("Female height in inches") +
  ggtitle("Histogram")


#histograms are easy
heights %>%
  filter(sex == "Female") %>%
  ggplot(aes(height)) +
  geom_density(fill = "orange")

#qqplots for different variables
params <- heights %>% filter(sex == "Male") %>%
  summarize(mean = mean(height), sd = sd(height))

##Normally the qq plot uses the sample parameter with mean 0 and sd 1
##when we use dparams then qq plot uses our actual variables
heights %>% filter(sex == "Male") %>%
  ggplot(aes(sample =  height)) +
  geom_qq(dparams = params) +
  geom_abline()

#we can do this with another method
heights %>%
  filter(sex=="Male") %>%
  ggplot(aes(sample = scale(height))) +
  geom_qq() +
  geom_abline()


#to make a quick qq plot we have to use the sample function
qplot(sample = scale(x)) + geom_abline()


heights %>% qplot(sex, height, data = ., geom="boxplot")

qplot(x, geom = "density")

heights %>%
  ggplot(aes(height,col = sex)) +
  geom_density()


