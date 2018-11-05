install.packages(c("CORElearn","e1071","ggplot2","GGally","ISLR","class","boot","glmnet","tree","caret","kernlab","klaR","C50","treebag","randomForest","gbm","rpart.plot","ROCR")) 

#Exercise
#Visualize the data set Titanic
summary(Titanic)
str(Titanic)
new_Titanic <- data.frame(Titanic)
str(new_Titanic)

plot(new_Titanic)
install.packages("GGally")
require(ggplot2)
require(GGally)
GGally::ggpairs(new_Titanic, aes(colour = Sex, alpha=0.4))

require(MASS)
require(ISLR)
summary(Carseats)
summary(Auto)
head(Auto)
str(Auto)

