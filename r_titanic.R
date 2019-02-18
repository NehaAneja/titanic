rm(list=ls())
data <- read.csv("E:/neha/DATA/titanic.csv")
View(data)

dim(data)
summary(data)
head(data)
tail(data)
dim(data$age)

?Titanic
mosaicplot(Titanic,main = "survival on titanic" , col = "red")
?mosaicplot

par(mfrow = c(1,2))
hist(data$age , col = "orange")
#NA TREATMENT
data$age[is.na(data$age)] = 29
summary(data)
par(mfrow =c (1,2))
hist(data$age , col="yellow")

bx1 <- boxplot(data$age)
#check cov


#create dummy of age and embarkment
data$female_s <- ifelse(data$sex == "female",1,0)
data$embarked_s <- ifelse(data$embarked == "s" ,1,0)
data$embarked_c <- ifelse(data$embarked == "c" ,1,0)
head(data)
summary(data)
View(data)

final_data <- data[-c(3,4,9)]
head(final_data)

#data ready 
#time for univariate analysis
#check outlier and linearity
bx <- boxplot(final_data$age)
bx$stats
#  [,1]
#[1,]    4
#[2,]   22
#[3,]   29
#[4,]   34
#[5,]   52

#outliers on both side #time for quantiles
quantile(final_data$age,seq(0,1,0.02))
par(mfrow=c(1,2))
# to remove 4% outliers 
final_data$age <- ifelse(final_data$age >=52,52,final_data$age)
final_data$age <- ifelse(final_data$age <=4,4,final_data$age)

bx <- boxplot(final_data$age)
quantile(final_data$age,seq(0,1,0.02))

#done hua outlier next kya?
#time to check outliers for fare
bx2 <- boxplot(final_data$fare)
bx2$stats
quantile(final_data$fare,seq(0,1,0.02))
#he have capped the values with 96th percentile,
#this case outlier is ver much we can not treat all outlier so we cap it to 6%
final_data$fare <- ifelse(final_data$fare >=136,136,final_data$fare)
bx3 <- boxplot(final_data$fare)
bx3$stats


#time for bivarite analysis
library(car)
library(carData)
scatterplot(final_data$age,final_data$survived)
scatterplot(final_data$fare,final_data$survived)
scatterplot(final_data$female_s,final_data$survived)

#regression time
set.seed(222)
