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


