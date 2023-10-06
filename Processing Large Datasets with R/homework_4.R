weight_data <- read.delim("weight_chart.txt")
weight


#1 - Draw a scatterplot (using geom_point) of the Age vs Weight.  When defining your aesthetics the Age will be the x and Weight will be the y.
data_weight <- weight_data %>% ggplot()
data_weight + geom_point(aes(x = weight_data$Age, y = weight_data$Weight))

#2Make all of the points filled with blue2 by putting a fixed aesthetic 
#into geom_point() and give them a size of 3

data_weight + geom_point(aes(x = weight_data$Age, y = weight_data$Weight), size = 3, color="blue")

#3- You will see that an obvious relationship exists between the two variables.  Change the geometry to geom_line to 
#see another way to represent this plot.

data_weight + geom_line(aes(x = weight_data$Age, y = weight_data$Weight), size = 3, color="blue")

#4- Combine the two plots by adding both a geom_line and a geom_point geometry to show both the individual points and the overall trend.

data_weight + geom_line(aes(x = Age, y = Weight), size = 3, color="blue") + geom_point(aes(x = Age, y = Weight), size = 3, color="red") 

g
