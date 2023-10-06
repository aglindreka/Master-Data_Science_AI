#1a)
v <- seq(1,10,0.5)

#1b)
c=0
fahrenheit_to_celsius <- function(a){
  c = (a ??? 32) * 5 ??? 9
  return(c)
}
fahrenheit_to_celsius(70)

#1c)



t <- sample(v, 100, replace = TRUE, prob = runif(length(v)))
which.max(table(t))

barplot(abs(t),col="yellow", main = "Sampling with replacements")

#1d)

mat <- matrix(rnorm(56, 1, 2), nrow=7, ncol = 8)
for(i in 1:7){
  for(j in 1:8){
    if (mat[i, j] < 1){
      mat[i, j] = 1
    }
  }
}
mat

#1e

data(iris)
head(iris)
library(dplyr)
data(iris)
iris_mod <- select(iris, Sepal.Length:Petal.Width)
iris_mod
pairs(iris_mod, col=(iris$Species))

#1f
#Show on the same plot, with different colours, the density functions of four random gamma 
#distribution with n =1000 and shape parameter respectively: 1, 2 , 3, 4. 
x = rgamma(1000,shape = 1)
y = rgamma(1000,shape = 2)
z = rgamma(1000,shape = 3)
d = rgamma(1000,shape = 4)
plot(density(x), col = "red") 
lines(density(y), col = "black") 
lines(density(z), col = "blue") 
lines(density(d), col = "orange") 

#2)
install.packages("nycflights13")
install.packages("tidyverse")
library(dplyr)
#2a)Create a new data frame that contains only the flights on 8 April 2013. 
#Find the flight with the lowest departure delay
head(nycflights13::flights)
new_dataFrame <- filter(nycflights13::flights, year == 2013 & month == 4 & day == 8)%>% filter(dep_delay == min(dep_delay, na.rm = TRUE))

#2b
#How many flights were delayed (on arrival or departure) by more than four hours?
filter(nycflights13::flights, arr_delay > 240)
filter(nycflights13::flights, arr_delay > 240| dep_delay > 240)
nrow(filter(nycflights13::flights, arr_delay > 240| dep_delay > 240))

#Create a new data frame containing the dep_delay variable for the flights with the highest departure delay in each month, 
#the month, the day and the departure delay monthly average. 
#select(nycflights13::flights, c(dep_delay, month, day)) %>% group_by(month)%>% summarize(delay = max(dep_delay, na.rm = TRUE), max_month = month, day = day, delay_mean = mean(dep_delay, na.rm = TRUE))
nycflights13::flights %>% group_by(month) %>% mutate(dep_delay_month = mean(dep_delay, na.rm = TRUE)) %>% filter(dep_delay == max(dep_delay, na.rm = TRUE)) %>% select(dep_delay, month, day, dep_delay_month)

#2c
#Considering only the flights that landed in LAX airport,
#show the departure delay and the arrival delay, ordered according to the departure delay in a descendent order. Then compute the column mean.

nycflights13::flights %>%filter(dest == 'LAX') %>% select(dep_delay, arr_delay)%>% arrange(desc(dep_delay))


####Terzo Esercizio

#3a) 

Age <- round(sample(runif(100, 20, 40)))
Weight = round(sample(runif(100, 50, 90)),1)
Graduated <- sample(c('YES','NO'),size=100,replace=TRUE, prob=c(0.60, 0.40))
data_people <- data.frame(Age, Weight, Graduated)

vec <- c("data_people$Age", "data_people$Weight", "data_people$Graduated")

#3.c
#Insert 5 missing values in each column at random locations, with a for loop.

for(i in 1:5){
  
    
  a <-sort(sample.int(100, 5))
  b <-sort(sample.int(100, 5))
  c <-sort(sample.int(100, 5))
  
  data_people$Age[a[i]] <- NA
  data_people$Weight[b[i]] <- NA
  data_people$Graduated[c[i]] <- NA
}


#3.d
data_people <- na.omit(data_people)
sum(is.na(data_people))
colnames(data_people)[3]  <- "Driving_License"
head(data_people)

#3e) Make a Min-Max Normalization: (X - min(X)) / (max(X) - min(X)) of the first two columns
library(dplyr)
data_people_col2 <- data_people[,c(1,2)]

normalize <- function(x){    
    (x - min(x)) / (max(x) - min(x))
}
data_people_norm1 <- apply(data_people_col2, 2, normalize)
data_people_norm1

#3f
data_people_colstat <- data_people[,c(1,2)] 

normalize <- function(x)
{    (x - mean(x))/sd(x)
}
data_people_colstat <- apply(data_people_colstat, 2, normalize)
data_people_colstat
