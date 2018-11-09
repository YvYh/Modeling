library(caret)
require(caret)
data=read.csv("./spamlearn.txt",header = TRUE, sep = "\t",dec = ".")
test=read.csv("./spamtest.txt",header = TRUE, sep = "\t",dec = ".")
metric="Accuracy"
preProcess=c("center", "scale")
set.seed(1)
control=trainControl(method="repeatedcv", number=2, repeats=5)
warnLevel=getOption("warn")
options(warn=-1)
# Logistic Regression
fit.glm <- train(Class~., data=data, method="glm", metric=metric, trControl=control)
pre.glm=predict(fit.glm,newdata = test)
rank.glm=confusionMatrix(table(test$Class, pre.glm))

#knn
fit.knn <- train(Class~., data=data, method="knn", metric=metric, preProc=preProcess, trControl=control)
pre.knn=predict(fit.knn,newdata = test)
rank.knn=confusionMatrix(table(test$Class, pre.knn))

#random forest
fit.rf <- train(Class~., data=data, method="rf", metric=metric, trControl=control)
pre.rf=predict(fit.rf, newdata = test)
rank.rf=confusionMatrix(table(test$Class,pre.knn))

# Linear Discriminant Analysis
fit.lda <- train(Class~., data=data, method="lda", metric=metric, preProc=preProcess, trControl=control)
pre.lda=predict(fit.lda, newdata = test)
rank.lda=confusionMatrix(table(test$Class,pre.lda))
