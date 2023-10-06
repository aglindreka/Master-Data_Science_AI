# 1a) Using the variable 'Sepal.Length" create an histogram.

library(ggplot2)
library(dplyr)
iris %>% ggplot() + geom_histogram(aes(x = Sepal.Length))

#1b) Draw the histogram for the variable "Sepal.Length" with 50  blue bins, 
#where the y-axis represents the densities.  Add a density red line to the plot. 

iris %>% ggplot() + geom_histogram(aes(x = Sepal.Length, y=..density..), bins = 50, fill = 'blue') + 
  geom_density(aes(x = Sepal.Length), color = "red")

#1c) Draw a scatterplot of Sepal.Length and Sepal.Width where colors and shapes depend on the Species
#                 1d) Add a separate regression line for each group. 
#                 1e) Then add an overall a smooth line (method = "loess")

iris %>% ggplot(aes(x = Sepal.Length, y = Sepal.Width)) + geom_point(aes(color = Species, shape = Species)) +
  geom_smooth(aes(color = Species, fill = Species),method = 'lm') + geom_smooth(method = 'loess', se = FALSE)

#1f Draw a separate scatter plot for each level of the variable Species with a regression line.
 
iris %>% ggplot(aes(x = Petal.Length, y = Petal.Width, color = Species, shape = Species)) +  geom_point(size = 3) + geom_smooth(method = "lm", aes(fill =Species)) +
  facet_wrap(facets =~ Species) 
  

#2

#2.1 From "mpg" dataset draw a scatter plot for `displ` (x-axis) and `hwy` (y-axis)
mpg %>% ggplot(aes(x = displ, y = hwy)) + geom_point()

#2.2 Modify the previous scatter plot in such a way that the color depends on the class and the shape on the year
mpg %>% ggplot(aes(x = displ, y = hwy)) + geom_point(aes(color = as.factor(class), shape = as.factor(year)))

#2c)display the same data conditionally on one categorical variable (here the class variable)- (one scatter plot per class)
mpg %>% ggplot(aes(x = displ, y = hwy)) + geom_point(aes(color = as.factor(class), shape = as.factor(year))) + 
  facet_wrap(facets =~ class)

#2d Load the diamonds dataset. Draw a scatter plot of 'carat' ` (x-axis)  and 'price' (y-axis) #
#where the colour depends on the variable 'cut', add also a smooth line ('lm') and display conditionally on the variable 'color'.
diamonds %>% ggplot(aes(x = carat, y = price, color = cut)) + geom_smooth(method = 'lm') + facet_wrap(facets =~ color)

#3a  Which variable (column) has the highest number of missing values?

dplyr::starwars 
sort(colSums(sapply(starwars,is.na)),decreasing = TRUE)[1]

#3b  How many humans contains the starwars dataset? show them by gender
starwars %>% filter(species == "Human") %>% group_by(gender) %>% summarize(count=n())

#3c
#From which homeworld do the most individuals (rows) come from? 
ma = starwars %>% group_by(homeworld) %>% summarize(count=n()) %>% arrange(desc(count))
ma[1,]

#3d) Create a barplot of the gender distribution of the starwars Universe, set the title : 
#Gender distribution of the sw Universe"#. Make the colors of the columns depend on the gender,
#modify the colour using the command : scale_fill_manual.

starwars %>% ggplot() + geom_bar(aes(x = gender, fill = gender)) scale_fill_manual = c("blue", "red", "yellow")# + 
  
  #scale_fill_manual(values=c("salmon2", "darkgoldenrod3", "chartreuse3"))
 
#3f) Draw the densities for the height variable of feminines and masculines only
starwars %>% group_by(gender) %>% na.omit() %>% ggplot(aes(height, fill = gender)) + geom_density()

#3 3g) Draw a segmented barplot for the variable 'sex'. The colors depend on the hair colours.
#Show the proportions [0,1] on the y-axis.

ggplot(starwars, aes(x = sex, fill = hair_color)) +
  geom_bar(position = "fill") +
  ylim(0.0, 1.0) +
  theme_bw()


