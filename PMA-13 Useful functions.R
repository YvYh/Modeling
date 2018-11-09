#### Predictive Modeling and Analytics
# Marko Robnik-Sikonja, November 2018

CA <- function(observed, predicted)
{
	t <- table(observed, predicted)

	sum(diag(t)) / sum(t)
}

brier.score <- function(observedMatrix, predictedMatrix)
{
	sum((observedMatrix - predictedMatrix) ^ 2) / nrow(predictedMatrix)
}

Sensitivity <- function(observed, predicted, pos.class)
{
	t <- table(observed, predicted)

	t[pos.class, pos.class] / sum(t[pos.class,])
}

Specificity <- function(observed, predicted, pos.class)
{
	t <- table(observed, predicted)
	neg.class <- which(row.names(t) != pos.class)

	t[neg.class, neg.class] / sum(t[neg.class,])
}

scale.data <- function(data)
{
	norm.data <- data

	for (i in 1:ncol(data))
	{
		if (!is.factor(data[,i]))
			norm.data[,i] <- scale(data[,i])
	}
	
	norm.data
}

#generate k-fold cross validation of n instances
cvGen<-function(n, k) {
  v <- 1:k
  vec <- array(1:k,dim=n)
  sample(vec, size=n)
}

# generate stratified k-fold cross validation partition based on classes in classVal
cvGenStratified<-function(classVal,k) {
  classVal<-factor(classVal)
  levs = factor(levels(classVal), levels=levels(classVal))
  classFreq <- table(classVal)
  noClasses <- length(levs)
  n <- length(classVal)
  srt <- order(classVal)
  vec <- array(1:k,dim=n)
  cv <- array(0,dim=n)
  cv[srt]<-vec
  for (i in 1:noClasses) 
    cv[classVal==levs[i]] <- sample(cv[classVal==levs[i]], size=classFreq[i], replace=F)
  cv
}