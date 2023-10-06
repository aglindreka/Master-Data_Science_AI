1)

31*78
678/41
x <- 39
y <- 22
z <- x-y
z
c <- sqrt(2345)
log2(c)

2)

vec1 <- c(2, 5, 8, 12, 16)
vec2 <- c(5:9)
vec1 - vec2
sequence <- seq(2, 100, 3)
sequence[c(5, 10, 15, 10)]
sequence[c(10,30)]

3)
library(dslabs)
data(murders)
a <- murders$abb 
class(a)
#"character"
b <- murders$abb[0:length(murders$abb)]
a[0:length(a)] == b[0:length(b)]
tab <- table(murders$region[murders$total])


4)
setwd("C:/Users/Aglind/Downloads")
data <- read.delim("small_file.txt")
data_csv <- read.csv("Child_Variants.csv")
head(data_csv)
View(data_csv)
mean(data_csv$MutantReadPercent)

5)
head(murders)
hist(murders$population)
boxplot(murders$population ~ murders$region, data = murders)

6)
Create a scatterplot with default parameters with the log transformed data.

brain <- read.delim("brain_bodyweight.txt", row.names=1)
brain_log2 <- log2(brain)
plot(brain_log2, brain, pch=19)

7)
num <- strtoi(readline(prompt="Enter number: "))
sum = 0
for(i in 1:10){
  sum = i*num
  print(sum)
}
#input 9

8)
tpn <- function(prime.num){
  if(prime.num <= 1){
    print("Not Prime")
  }
  else if(prime.num == 2){
    print("Prime")
  } else {
    
    if(prime.num%%(2:(prime.num - 1)) == 0){
      print("Not Prime")
      
    } else { 
      print("Prime")
      
    }
  }
}
tpn(199)

