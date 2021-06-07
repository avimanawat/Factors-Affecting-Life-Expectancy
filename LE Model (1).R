setwd("C:\\Users\\punee\\OneDrive\\Desktop\\R final submission")


options(max.print=1000000)

lifeExp = read.csv("life expectancy clean.csv", header = TRUE)


lifeExp=lifeExp[-c(9,15)]
lifeExp=lifeExp[!grepl("Libya", lifeExp$Country),]
lifeExp=lifeExp[!grepl("Zimbabwe", lifeExp$Country),]



sum(is.na(lifeExp))


for(i in unique(lifeExp$BMI.Indexing)) {
  lifeExp[paste("BMI.Index",i,sep="_")] = ifelse(lifeExp$BMI.Indexing==i,1,0)
}

          
nrow(lifeExp)


####################################### Splitting the data ############################

lifeExp=lifeExp[lifeExp[,21]>0,]

dt = sort(sample(nrow(lifeExp),nrow(lifeExp)*.8))  #random data

trainingData = lifeExp[dt,]
testData = lifeExp[-dt,]


sum(is.na(trainingData))

cor(trainingData[-c(1,3,22)])  #correlation Matrix


################################ Rejected Model #################################################

myModel = lm((trainingData$Life.expectancy) ~ trainingData$Schooling + trainingData$HIV.AIDS + trainingData$Diphtheria + trainingData$Polio
             +trainingData$BMI + sqrt(trainingData$Adult.Mortality))
summary(myModel)
anova(myModel)
library(MASS)
library(car)
vif(myModel)
qqnorm(myModel$residuals)
qqline(myModel$residuals, col = 2)



################################ Best MOdel ######################################################


model=lm((Life.expectancy)~HIV.AIDS+Status+Polio+(GDP)+BMI.Index_Overweight+BMI.Index_Underweight+Schooling+Adult.Mortality, data =trainingData )


summary(model)
anova(model)

library(MASS)
library(car)
vif(model)



AIC(model)
BIC(model)




##################### Residual Plots ##########################################

qqnorm(model$residuals)
qqline(model$residuals, col = 2)


hist(model$residuals)

plot(model$fitted.values,model$residuals)
abline(h=0, col= 'red')

plot(trainingData$HIV.AIDS,model$residuals)
abline(h=0, col= 'red')

plot((trainingData$GDP),model$residuals)
abline(h=0, col= 'red')

plot(trainingData$Schooling,model$residuals)
abline(h=0, col= 'red')

plot(trainingData$Adult.Mortality,model$residuals)
abline(h=0, col= 'red')



par(mfrow = c(2,2))
plot(model)

library(car)
durbinWatsonTest(model)


########################## Errors #################################################


predictionSum=sum(predict(model, testData))
actualSum=sum(testData$Life.expectancy)

errorPer=abs(predictionSum-actualSum)*100/actualSum
print(errorPer)




###########################  Cross Validation ###########################################


model1=glm((Life.expectancy)~HIV.AIDS+Status+Polio
         +(GDP)+BMI.Index_Overweight+BMI.Index_Underweight+Schooling+Adult.Mortality, data =trainingData )
install.packages("boot")
library(boot)
set.seed(4)
crossv = cv.glm(trainingData, model1, K =5)
crossv$delta


########################## Advance Machine Learning ####################################

install.packages("caret")
library(caret)


validation_index <- createDataPartition(lifeExp$Life.expectancy, p=0.80, list=FALSE)
# select 20% of the data for validation
validation <- lifeExp[-validation_index,]
# use the remaining 80% of data to training and testing the models
lifeExp <- lifeExp[validation_index,]
#Overview of algos supported by caret
names(getModelInfo())



control <- trainControl(method="cv", number=10)
metric <- "RMSE"
# decision tree
set.seed(7)
fit.cart <- train(Life.expectancy~., data=lifeExp, method="rpart",metric=metric, trControl=control)
# Random Forest
set.seed(7)
fit.rf <- train(Life.expectancy~., data=lifeExp, method="rf",metric=metric, trControl=control)
yes# kNN
set.seed(7)
fit.knn <- train(Life.expectancy~., data=lifeExp, method="knn",metric=metric, trControl=control)
# SVM
set.seed(7)

fit.svm <- train(Life.expectancy~., data=lifeExp, method="svmRadial",metric=metric, trControl=control)
#neural net
set.seed(7)
fit.nn <- train(Life.expectancy~., data=lifeExp, method="nnet",metric=metric, trControl=control)





