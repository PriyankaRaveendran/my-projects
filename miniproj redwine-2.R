library(ggplot2)
library (gridExtra)
library(corrplot)
#load the wine quality dataset
redwine <- read.csv("C:/Users/Keshi/Downloads/redwine.csv")
View(redwine)
redwine<-data.frame(redwine)
str(redwine)
summary(redwine)
#histogram analysis
#plot shows that quality 5 is more
qplot(redwine$quality, geom="histogram") 

#plot shows that alcohol
summary(redwine$alcohol)
#"alcohol" is right-skewed distributed with some outliers located at right side. The most frequent values are between 9.4-9.6. IQR is 1.6.
qplot(redwine$alcohol, geom="histogram") 
#Now the IQR is 1.4, 3rd quantile is not very far from max value, and data gathers more in center.
qplot(alcohol, data=redwine, fill=I('#F79420'), binwidth = .1)+xlim(9.50,11.10 )

#We can see majority fixed acidity gathering in middle part
qplot(redwine$fixed.acidity, geom="histogram") 

# The most frequent values are between 0.4-0.6. IQR is 0.25.right skewed
qplot(redwine$volatile.acidity, geom="histogram") 

qplot(redwine$citric.acid, geom="histogram") 
#summary
summary(redwine$citric.acid)
qplot(citric.acid, data=redwine, fill=I('#F79420'), xlim(0.1,0.4))
#Ater removing outliers.The most common citric.acid values are around 0.49
qplot(citric.acid, data=redwine, fill=I('#F79420'), binwidth = .01)+xlim(0.090,.5)

qplot(redwine$chlorides, geom="histogram") 
qplot(chlorides, data=redwine, fill=I('#F79420'), binwidth = .01)+ xlim(.012, .2)


qplot(redwine$pH, geom="histogram") 
qplot(pH, data=redwine, fill=I('#F79420'), binwidth = .01)+xlim(3.210, 3.400)

qplot(redwine$density, geom="histogram") 
qplot(density, data=redwine, fill=I('#F79420'), binwidth = .001)



#The residual sugar content is high between 1.9 to 2.6. Residual sugar has a positively skewed distribution; even after eliminating the outliers distribution will remain skewed
qplot(redwine$residual.sugar, geom="histogram")
qplot(residual.sugar, data=redwine, fill=I('#F79420'), binwidth = .1)+xlim(1.5,5)

qplot(redwine$free.sulfur.dioxide, geom="histogram") 
qplot(redwine$total.sulfur.dioxide, geom="histogram") 

# Overall distribution

#A distribution analysis of the key paramaters pH,Total and Free sulfur.dioxide and Alcohol is explored, as these are the key factors 

cor(redwine)
attach(redwine)
cor(quality,alcohol)
cor(quality,pH)
cor(quality,sulphates)
cor(volatile.acidity,quality)
cor(quality,citric.acid)
#correlation is high for alcohol and quality





#boxplot analysis
boxplot(redwine$alcohol,main="Avg no.of rooms",col="red")
boxplot(quality,alcohol,col = c("red","blue"),xlab="ozone depletion")
ggplot(aes(factor(quality), 
           alcohol), 
       data = redwine) +
  geom_jitter( alpha = .3)  +
  geom_boxplot( alpha = .5,color = 'blue')
ggplot(aes(factor(quality), 
           pH), 
       data = redwine) +
  geom_jitter( alpha = .3)  +
  geom_boxplot( alpha = .5,color = 'blue')
ggplot(aes(factor(quality), 
           citric.acid), 
       data = redwine) +
  geom_jitter( alpha = .3)  +
  geom_boxplot( alpha = .5,color = 'blue')
ggplot(aes(factor(quality), 
           volatile.acidity), 
       data = redwine) +
  geom_jitter( alpha = .3)  +
  geom_boxplot( alpha = .5,color = 'blue')
ggplot(aes(factor(quality), 
           sulphates), 
       data = redwine) +
  geom_jitter( alpha = .3)  +
  geom_boxplot( alpha = .5,color = 'blue')
ggplot(aes(factor(quality), 
           residual.sugar), 
       data = redwine) +
  geom_jitter( alpha = .3)  +
  geom_boxplot( alpha = .5,color = 'blue')
ggplot(aes(factor(quality), 
           density), 
       data = redwine) +
  geom_jitter( alpha = .3)  +
  geom_boxplot( alpha = .5,color = 'blue')


r<-lm(formula = quality ~alcohol, data = redwine)
r
summary(r)
anova(r)

# Create Training and Test data -
set.seed(100)  # setting seed to reproduce results of random sampling
model <- sample(1:nrow(redwine), 0.8*nrow(redwine))  # row indices for training data
trainingData <- redwine[model, ]  # model training data
testData  <- redwine[-model, ]   # test data
lmMod <- lm(quality ~ alcohol, data=trainingData)  # build the model
Pred <- predict(lmMod, testData)
Pred
summary (lmMod) 

actuals_preds <- data.frame(cbind(actuals=testData$quality, predicteds=Pred))
correlation_accuracy <- cor(actuals_preds)  # 82.7%
correlation_accuracy
head(actuals_preds)
plot(testData$quality,type='l',lty=1.8,col="green")
lines(Pred,type='l',col="blue")
summary(r)
anova(r)
error <- r$residuals  # same as data$Y - predictedY
predictionRMSE <- rmse(error) 
predictionRMSE 
#rmse value is 0.7 the model is not good so we go for classification




#logistic regression
#introduce new attribute 
redwine$type<-ifelse(redwine$quality>5,1,0)
str(redwine)
redwine$type <- factor(redwine$type, ordered = T)
View(redwine)
#split function
library(caTools)
set.seed(88)
split <- sample.split(redwine$type, SplitRatio = 0.75)
#train and test data
train <- subset(redwine, split == TRUE)
test <- subset(redwine, split == FALSE)
#logistic regression model
attach(redwine)
model <- glm (type ~ ., data = train, family=binomial)
summary(model)
predict <- predict(model,test, type = 'response')
#confusion matrix
conf<-table(train$type, predict>0.5)
conf
#ROC curve(fpr=x axis,tpr=y axis)
library(ROCR)
ROCRpred <- prediction(predict, train$type)

ROCRperf <- performance(ROCRpred, 'tpr','fpr')
plot(ROCRperf, colorize = TRUE, text.adj = c(1,10))
#glm plot
library(ggplot2)
ggplot(train, aes(x=alcohol, y=type)) + geom_point() + stat_smooth(method="glm", family="binomial", se=FALSE)

ggplot(train, aes(x=alcohol, y=type) )+ 
  geom_point(alpha=.5) +
  stat_smooth(method="glm", se=FALSE, method.args = list(family=binomial)) + 
  ylab("type") 


library(ggplot2)
ggplot(train, aes(x=alcohol, y=type)) + geom_point() + 
  stat_smooth(method="glm", family="binomial", se=FALSE)






# converting quality into a binary factor
for (i in 1:nrow(redwine)) {
  if (redwine$quality[i] > 5)
    redwine$label[i] <- 1
  else
    redwine$label[i] <- 0
}
redwine$label <- factor(redwine$label, levels = c(0, 1), labels = c("bad", "good"))
# removing the quality and type variable
redwine$quality <- NULL
redwine$type<- NULL


# using a subset of 1000 obs for the training set
test_indices <- sample(1:nrow(redwine), 1000)
test <- redwine[test_indices,]
train <- redwine[-test_indices,]

#knn
set.seed(10)
library(class)
# using 20 nearest neighbors
knn_pred <- knn(train = train[,-12], 
                test = test[,-12], 
                cl = train$label, 
                k = 35, prob = TRUE)
View(redwine)
# confusion matrix
knn_conf <- table(pred = knn_pred, true = test$label)
knn_conf
library(DTComPair)

# Creating the ROC curve for knn
# library(dplyr) is loaded
knn_prob <- attr(knn_pred, "prob")
knn_prob <- 2 * ifelse(knn_pred == "-1", 1-knn_prob, knn_prob) - 1
knn_roc_pred <- prediction(predictions = knn_prob, labels = test$label)
knn_roc_perf <- performance(knn_roc_pred, 
                            measure = "tpr", x.measure = "fpr")

# Plotting the KNN ROC curve
plot(knn_roc_perf, col = 2, lwd = 3, 
     main = "ROC Curve for kNN, k = 10")
abline(0,1)
# Area under the knn curve
knn_auc_perf <- performance(knn_roc_pred, measure = "auc")
knn_AUC <- knn_auc_perf@y.values[[1]]
knn_AUC



# Building the ROC Curve
rf_pred <- as.data.frame(predict(rf, newdata = test, type = 'prob'))
rf_pred_probs <- rf_pred[,2]
rf_roc_pred <- prediction(rf_pred_probs, test$label)
rf_perf <- performance(rf_roc_pred, measure = "tpr", 
                       x.measure = "fpr")

# Plotting the curve
plot(rf_perf, col = 2, lwd = 3, 
     main = "ROC Curve for randomForest with 8 variables")
abline(0,1)

rf_perf2 <- performance(rf_roc_pred, measure = "auc")
rf_AUC <- rf_perf2@y.values[[1]]
rf_AUC
rf2 <- randomForest(formula = label ~ alcohol + volatile.acidity + free.sulfur.dioxide,
                    data = train,
                    mtry = 3)
# predicting on the test set
rf_pred2 <- predict(rf2, test, type = "class")

# Confusion Matrix
rf_conf2 <- table(test$label, rf_pred2)
rf_conf2
# Building the ROC1 Curve
rf_pred2 <- as.data.frame(predict(rf2, test, type = 'prob'))
rf_pred_probs2 <- rf_pred2[,2]
rf_roc_pred2 <- prediction(rf_pred_probs2, test$label)
rf_perf2 <- performance(rf_roc_pred2, 
                        measure = "tpr", 
                        x.measure = "fpr")

# Plotting the curve
plot(rf_perf2, col = 2, lwd = 3, 
     main = "ROC Curve for randomForest with 3 variables")
abline(0,1)
# Area under the curve
rf_perf22 <- performance(rf_roc_pred2, measure = "auc")
rf_AUC2 <- rf_perf22@y.values[[1]]
rf_AUC2




#random forest
library(randomForest)
library(class)
rf <- randomForest(formula = label ~ .,
                   data = train,
                   mtry = 8)

print(rf)
varImpPlot(rf, main = "Variable Importance Plot")
# predicting on the test set
rf_pred <- predict(rf, test, type = "class")

# Confusion Matrix
rf_conf <- table(true = test$label, pred =  rf_pred)
rf_conf

# Building the ROC Curve
rf_pred <- as.data.frame(predict(rf, newdata = test, type = 'prob'))
rf_pred_probs <- rf_pred[,2]
rf_roc_pred <- prediction(rf_pred_probs, test$label)
rf_perf <- performance(rf_roc_pred, measure = "tpr", 
                       x.measure = "fpr")

# Plotting the curve
plot(rf_perf, col = 2, lwd = 3, 
     main = "ROC Curve for randomForest with 8 variables")
abline(0,1)

rf_perf2 <- performance(rf_roc_pred, measure = "auc")
rf_AUC <- rf_perf2@y.values[[1]]
rf_AUC
rf2 <- randomForest(formula = label ~ alcohol + volatile.acidity + free.sulfur.dioxide,
                    data = train,
                    mtry = 3)
# predicting on the test set
rf_pred2 <- predict(rf2, test, type = "class")

# Confusion Matrix
rf_conf2 <- table(test$label, rf_pred2)
rf_conf2
# Building the ROC1 Curve
rf_pred2 <- as.data.frame(predict(rf2, test, type = 'prob'))
rf_pred_probs2 <- rf_pred2[,2]
rf_roc_pred2 <- prediction(rf_pred_probs2, test$label)
rf_perf2 <- performance(rf_roc_pred2, 
                        measure = "tpr", 
                        x.measure = "fpr")

# Plotting the curve
plot(rf_perf2, col = 2, lwd = 3, 
     main = "ROC Curve for randomForest with 3 variables")
abline(0,1)
# Area under the curve
rf_perf22 <- performance(rf_roc_pred2, measure = "auc")
rf_AUC2 <- rf_perf22@y.values[[1]]
rf_AUC2

