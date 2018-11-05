library(MASS)
library(ISLR)
names(Boston)
lm.fit=lm(medv~lstat,data=Boston)
lm.fit
#all columns become valuable
attach(Boston)
lm.fit=lm(medv~lstat)
summary(lm.fit)
names(lm.fit)

lm.fit=lm(medv~.,data=Boston)
summary(lm.fit)
#formula interface:
# + add to the model
# . include all
# - exclude from the model

?Carseats

#exercise
names(Carseats)
summary(lm(Sales~Income*Advertising*Population*ShelveLoc*Age*Education))
lm.fit1=lm(Sales~Income,data = Carseats)
lm.fit2=lm(Sales~log(Income),data = Carseats)
anova(lm.fit1,lm.fit2)

summary(lm(Sales~.
           -Population
           -Education
           -US
           -Urban
           +Advertising:Income:Population
           +Advertising:Population:Education
           ,data=Carseats))


K-fold <- function(data, k){
  listModel <- list()
  errorMin <- 10000
  indexMin <- 0
  for (i in 1:k) {
    trainingI <- sample(nrow(data),0.7*nrow(data))
    subTrain <- data[trainingI,]
    validate <- data[-trainingI,]
    lm.fit <- lm(mpg~.,data = subTrain)
    listModel[[i]] <- summary(lm.fit)
    cv.error <- mean((validate$mpg-predict(lm.fit,validate)))
    if ((cv.error^2) < errorMin) {
      errorMin <- cv.error
      indexMin <- i
    }
  }
  sort(listModel[[indexMin]]$coefficients[,4])#Pr(>|t|) 
}

