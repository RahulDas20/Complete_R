library(tidyverse)
library(dslabs)
data("gapminder")
gapminder %>%
  as_tibble()


gapminder %>%
  filter(year %in% c(1962,2012)) %>%
  ggplot(aes(fertility, life_expectancy,colour = continent)) +
  geom_point() +
  facet_grid(.~year)


years <- c(1962,1980,1990,2000,2012)
continents <- c("Europe","Asia")

gapminder %>%
  filter(year %in% years & continent %in% continents) %>%
  ggplot(aes(fertility,life_expectancy,col=continent)) +
  geom_point() +
  facet_wrap(~year)

filter(gapminder, year %in% c(1962,2012)) %>%
  ggplot(aes(fertility, life_expectancy, col = continent)) +
  geom_point() +
  facet_grid(.~year)


#adding a new variable to gapminder data
gapminder <- gapminder %>% mutate((dollars_per_day <- gdp/population/365))

gapminder

past_year <- 1970

gapminder %>% 
  filter(year == past_year & !is.na(gdp)) %>%
  ggplot(aes(dollers_per_day)) +
  geom_histogram()



# Load necessary library
library(ggplot2)



