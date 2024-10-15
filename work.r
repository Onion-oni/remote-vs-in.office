## SAVE DATA AS DF ##
df <- read.csv('remote_work_productivity.csv')
## CHECK HEAD OF DATA FRAME ##
head(df)
## SINCE WE WON'T BE NEEDING TO USE EMPLOYEE_ID WE CAN REMOVE IT
df <- df[,-1]
head(df)
## CHECK FOR ANY MISSING DATA ##
any(is.na(df))
any(is.null(df))
## EXPLORATORY DATA ANALYSIs ##
# SOME OF THESE VALUES EXCEED THE 0-100 SCALE #
library(dplyr)
(filter(df,Well_Being_Score>100))
(filter(df,Productivity_Score>100))

##FOR ANY VALUES EXCEEDING 100 WE WILL REPLACE THEM WITH 100##

df$Well_Being_Score[df$Well_Being_Score > 100] <- 100
df$Productivity_Score[df$Productivity_Score > 100] <- 100

## GRAPH THE DATA ##
library(ggplot2)
pl1 <- ggplot(data = df,aes(Hours_Worked_Per_Week,Productivity_Score)) + geom_point(aes(color=Employment_Type)) + theme_bw()
print(pl1)
pl2 <- ggplot(data = df,aes(Hours_Worked_Per_Week,Well_Being_Score)) + geom_point(aes(color=Employment_Type)) + theme_bw()
print(pl2)
pl3 <- ggplot(data = df,aes(Well_Being_Score,Productivity_Score)) + geom_point(aes(color=Employment_Type)) + theme_bw()
print(pl3)



## USING KNN TO PREDICT WHETHER SOMEONE WORKS REMOTE OR IN OFFICE ##
library(class) 
head(df)

#GRABBING A SAMPLE SIZE FOR DATA#
test.index <- 1:750

employment.type <- df[,1]
standardized.df <- scale(df[,-1])

#MAKING TESTING AND TRAINING DATA#

test.data <- standardized.df[test.index,]
test.employment.type <- employment.type[test.index]

train.data <- standardized.df[-test.index,]
train.employment.type <- employment.type[-test.index]

#KNN MODEL#
set.seed(17)
predicted.employment.type <- knn(train.data,test.data,train.employment.type,k=16)
head(predicted.employment.type)

#REVIEWING MISCLASSIFICATION ERRORS#
misclass.error <-mean(test.employment.type != predicted.employment.type)
print(misclass.error)

#CHOOSING A K-VALUE BASED ON THE LOWEST NUMBER OF MISCLASS ERRORS#
predicted.employment.type <- NULL
error.rate <- NULL

for (i in 1:20){
  set.seed(17)
  predicted.employment.type <- knn(train.data,test.data,train.employment.type,k=i)
  error.rate[i] <- mean(test.employment.type != predicted.employment.type)
}

print(error.rate)

## VISUALIZE K ELBOW METHOD ##
library(ggplot2)
k.values <- 1:20
error.df <- data.frame(error.rate,k.values)

print(error.df)

error.pl <- ggplot(error.df,aes(k.values,error.rate)) + geom_point() + geom_line(lty='dotted',color='red')
print(error.pl)

#k=16 HAS THE LOWEST ERROR RATE#

##CORRPLOT##
df.nums <- sapply(df, is.numeric)
cor.data <- cor(df[,df.nums])

corrplot(cor.data,method='color')
#THERE SEEMS TO BE A SMALL CORRELATION BETWEEN PRODUCTIVITY AND WELLBEING#
