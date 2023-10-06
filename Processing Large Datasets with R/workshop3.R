#Download the dataset : winequality-red.csv and show the summary.

library(dplyr)
library(ggplot2)
red_wine <- read.csv('winequality-red.csv', sep=';')

summary(red_wine)

#Add the categorical variable "label" grouping the alcohol variable in the following way:
#Light: alcohol < 9; Medium: 9 , alcohol < 12 ; Strong: alcohol  12

red = red_wine %>% group_by(alcohol) %>% mutate(label = 1)

red$label[red$alcohol < 9] = 'Light'
red$label[red$alcohol >= 9 & red$alcohol < 12] = 'Medium'
red$label[red$alcohol >= 12] = 'Strong'

#How many Light, Medium and Strong wines there are in the dataset?
  
table(red$label)

#Create a barplot of the 3 levels variable "label" where the color of bars depends on the level  of the alcohol concentration, with title: "Alcohol concentration"
red %>% ggplot(aes(factor(label))) + geom_bar(aes(fill = factor(label))) + ggtitle("Alcohol concentration")

#Show histograms for fixed.acidity by label (Light, Medium, Strong)
red %>% ggplot() + geom_histogram(aes(x = fixed.acidity)) + facet_wrap(~ label)
#facet_wrap(~ label) -> We use facet_wrap when I want to see a categorial variable depending on categories

#Create a density plot (geom_density()) showing the citric.acid of the three level of the variable "label"
red %>% ggplot(aes(citric.acid)) + geom_density(aes(fill = label))

#Create a scatter plot quality and alcohol where the color of points varies depending on the following quality
#ranges (2, 4], (4, 6], (6, 8]. Add a dashed line for the median. Give a title to the plot and to the x and y axes
vec = red$quality
vec[red$quality  <= 4] = 1
vec[red$quality  >  4 & red$quality  <= 6] = 2
vec[red$quality  >  6 & red$quality  <= 8] = 3
table(red$quality)
vec
red %>% ggplot(aes(x=quality, y=alcohol)) + geom_jitter(color=vec) + ggtitle("Plot points") + xlab('Quality') + ylab('Alcohol') +
  stat_summary(fun = median, geom = "line", linetype = 2, color = "steelblue") 


#exercise 2
#Load the dataset gapminder from the package gapminder
library(gapminder)
head(gapminder)

#Create a tibble 5 x 4 containing the number of observation per continent, the number 
#of countries per continent and the average of life expectation.
cont <- gapminder %>% group_by(continent) %>% summarise(n_continent = n(), n_country = length(unique(country)), av = mean(lifeExp))
cont
#which.max(cont)

#What was the most populous European country in 1992?
a = gapminder %>% filter(continent == 'Europe' & year == '1992')
a$country[which.max(a$pop)]

#Modify the population variable by dividing it by 10^6. Show the results sorted according to population in a descendent order.
gapminder %>% mutate(population  = pop/10^6) %>% arrange(desc(pop))

#Draw a scatterplot of gdpPercap and lifeExp where the color of points depends on 
#the continent. Overall also a smooth line (loess) with it's confidence intervals. 
#Give the plot a title and axis labels.

gapminder %>% ggplot(aes(x = log(gdpPercap), y = lifeExp)) + geom_point(aes(color = continent)) + ggtitle("Continents") + xlab('Gdp per Cap') +
  geom_smooth(method = loess)


#Exercise 3

library(tidyverse)
library(janitor)
wake <- read_csv("https://www2.stat.duke.edu/~sms185/data/econ/parcels.csv")

#Which city has the fewest city decode in the dataset?
q1 <- wake %>% count(CITY_DECODE)
q1%>%arrange(n) %>%slice(1)

#Compute the mean area (calc_area) for each design style
l <- wake %>% group_by(DESIGNSTYL) %>% summarise(men = mean(CALC_AREA))

#Compute the median sale price (totsalprice) for each year (sale_date).Hint:lubridate::year()

t <- wake %>% group_by(lubridate::year(SALE_DATE)) %>% summarise(p = median(TOTSALPRICE))
