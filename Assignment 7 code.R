#install.packages("readxl")
install.packages("ROSE")
install.packages("VIM")
install.packages("VIF")
install.packages("DEoptimR", dependencies = TRUE)
install.packages("nnet")
install.packages("ggvis")
install.packages("class")
install.packages("car")

library(caret)
library(car)
library(VIF)
library(readxl)
library(xlsx)
library(ROSE)
library(VIM)
library(nnet)
library(devtools)
library(ggvis)
library(class)
library(plyr)


chu <- read.csv(file.choose(),header=T)
View(chu1)
str(chu)
#convert numeric data to factor
chu$Churn..1...Yes..0...No. <- as.factor(chu$Churn..1...Yes..0...No.)

# check for na values
sum(is.na(chu))


# check the balance of the dependent variable
table(chu$Churn..1...Yes..0...No.)

# Proportion of 0's and 1's
prop.table(table(churn$Churn..1...Yes..0...No.))

#over sampling
#churn_balanced_over <- ovun.sample(Churn..1...Yes..0...No. ~ ., data = churn, method = "over",N = 12048)$data
#table(churn_balanced_over$Churn..1...Yes..0...No.)


#using undersampling and oversampling together
balancechu<- ovun.sample(Churn..1...Yes..0...No. ~ ., data = chu, method = "both", p=0.5,N=6347, seed = 1)$data

# checking values after balancing 
table(balancechu$Churn..1...Yes..0...No.)


#str(churn_balanced_both)
#drops <- c("Churn..1...Yes..0...No.")
#churn_balanced_both1 <-churn_balanced_both[ , !(names(churn_balanced_both) %in% drops)]

View(balancechu)

# Scaling the dataset 


maxs = apply(balancechu[ , c(2, 4:13)], 2, max)
mins = apply(balancechu[ , c(2, 4:13)], 2, min)
scaled_data = as.data.frame(scale(balancechu[ , c(2, 4:13)], center = mins, scale = maxs - mins))

churn = data.frame(scaled_data,balancechu$Churn..1...Yes..0...No.)
churn = rename(churn,c("balancechu.Churn..1...Yes..0...No."="result"))

#str(churn)
View(churn)

#partion the dataset for train and testing
set.seed(999)
ind = sample(2, nrow(churn), replace = T, prob = c(0.7, 0.3))
traind = churn[ind == 1, ]
testd = churn[ind == 2, ]

# Check for balance again
table(traind$result)
table(testd$result)

View(traind)

#random forrest model

library(randomForest)
attach(churn)
forest = randomForest(result ~ Customer.Age..in.months.+SP.Month.0+CHI.Score.Month.0+CHI.Score.0.1+Support.Cases.Month.0+Support.Cases.0.1+Logins.0.1+Blog.Articles.0.1+Views.0.1+Days.Since.Last.Login.0.1 , data = TrainData, ntree = 100, mtry = 3, proximity = TRUE, replace = TRUE, sampsize = ceiling(0.65*nrow(TrainData)) , importance = TRUE )

print(rf)

pred = predict(forest, newdata = traind)
table(pred, traind$result)


pred2 = predict(forest, newdata = testd)
table(pred2, testd$result)


ticPred1 = predict(rf, newdata = TestData)
table(ticPred1, TestData$rate)

#my logistic
# Logistic model with one and all variables as null and full respectively
null = glm(result ~ Customer.Age..in.months., data= traind, family = "binomial") 
full = glm(result ~., data= traind, family = "binomial") 

# performing logistic using both forward and backward 
Log_model <- step(null, scope = list(upper=full), data=traind, direction="both")
summary(Log_model)

# Predictions on the traindata
pred_train = predict(Log_model,newdata =traind, type = 'response')
pred_train_matrix <- table(traind$result, pred_train>0.5)
pred_train_matrix
# accuracy on traindata
accuracy<-sum(diag(pred_train_log_matrix))/sum(pred_train_log_matrix)
accuracy

# Predictions on the testdata
pred_test= predict(Log_model,newdata = testd, type = 'response')
pred_test_matrix <- table(testd$result, pred_test>0.5)
pred_test_matrix
# accuracy on the testData
accuracy<-sum(diag(pred_test_matrix))/sum(pred_test_matrix)
accuracy

# Logistic model with one and all variables as null and full respectively
null = glm(response ~ Customer.Age..in.months., data= TrainData, family = "binomial") 
full = glm(response ~., data= TrainData, family = "binomial") 

# performing logistic using both forward and backward 
Logistic_model <- step(null, scope = list(upper=full), data=TrainData, direction="both")
summary(Logistic_model)

# Predictions on the traindata
pred_train_log = predict(Logistic_model,newdata =TrainData, type = 'response')
pred_train_log_matrix <- table(TrainData$response, pred_train_log>0.5)
pred_train_log_matrix
# accuracy on traindata
accuracy<-sum(diag(pred_train_log_matrix))/sum(pred_train_log_matrix)
accuracy

# Predictions on the testdata
pred_test_log<- predict(Logistic_model,newdata = TestData, type = 'response')
pred_test_log_matrix <- table(TestData$response, pred_test_log>0.5)
pred_test_log_matrix
# accuracy on the testData
accuracy<-sum(diag(pred_test_log_matrix))/sum(pred_test_log_matrix)
accuracy

# Best fit logistic model
null = glm(rate ~ Customer.Age..in.months., data= TrainData, family = "binomial") # only includes one variable 
full = glm(rate ~., data= TrainData, family = "binomial") # includes all the variables > # We can perform forward selection using the command: 
#step(null, scope=list(lower=null, upper=full), direction="forward")
#step(full, data=TrainData, direction="backward")  
step(null, scope = list(upper=full), data=TrainData, direction="both")


# Final model selected AIC 5831

logmodel <- glm(formula = rate ~ Customer.Age..in.months. + CHI.Score.Month.0 + 
      Days.Since.Last.Login.0.1 + CHI.Score.0.1 + Views.0.1 + 
      Logins.0.1 + Support.Cases.Month.0 + Support.Cases.0.1, family = "binomial", data =TrainData)
summary(logmodel)



logModel1 <- glm(rate ~ Customer.Age..in.months. + 
                  CHI.Score.Month.0 + CHI.Score.0.1 + SP.Month.0 + Support.Cases.0.1 + 
                  Support.Cases.Month.0 + Logins.0.1, family = "binomial",data = TrainData)
                   
summary(logModel1)

# Check for multicollinearity 
vif(Log_model)

sqrt(vif(Log_model))>2


# Predictions on the testdata
pred<- predict(logmodel,newdata = TestData)
pred

#prediction on the complete model
glm.probs = predict(logmodel,newdata =TrainData, type = 'response')

glm.probs[1:10]

# Assigning 1 and 0 based on probability
contrasts(TrainData$rate)
glm.pred=rep('0',4472)
glm.pred[glm.probs>.5]='1'
table(glm.pred,TrainData$rate)

finaltraindata=cbind(TrainData, glm.pred)


glm.probs = predict(logmodel,TestData,type = 'response')
glm.probs[1:10]
contrasts(TestData$rate)

glm.pred=rep('0',1875)
glm.pred[glm.probs>.5]='1'
glm.pred
table(glm.pred)

table(glm.pred,TestData$rate)
finaldata=cbind(TestData, glm.pred)



# RoC plot 
rocplot(logModel)


nn = nnet(result ~ ., data=traind, linout=F, size=10, decay=0.01, maxit=1000)
summary(nn)

nn.preds = predict(nn, testd, type = "class")
nn.preds
nn.preds.matrix= table(testd$result, nn.preds)
nn.preds.matrix

accuracy<-sum(diag(nn.preds.matrix))/sum(nn.preds.matrix)
accuracy

sort(nn$fitted)
nn$wts



nn.preds_test = predict(nn, traind, type = "class")
nn.preds_test_mat=table(traind$result, nn.preds_test)

accuracy<-sum(diag(nn.preds_test_mat))/sum(nn.preds_test_mat)
accuracy



View(churn)

set.seed(1234)
ind = sample(2, nrow(churn), replace=TRUE, prob=c(0.7, 0.3))
churn.training = churn[ind==1,1:11]
churn.test = churn[ind==2, 1:11]

churn.trainLabels = churn[ind==1, 12]
churn.testLabels = churn[ind==2, 12] 

str(churn.trainLabels)
str(churn.training)

KNN_pred = knn(train = churn.training, test = churn.test, cl=churn.trainLabels, k=3)

KNN_pred
table(KNN_pred,churn.testLabels)

# checking on balanced trained data
set.seed(1234)
ind = sample(2, nrow(churn), replace = T, prob = c(0.7, 0.3))
TrainData = churn[ind == 1, ]
TestData = churn[ind == 2, ]
churn$Churn..1...Yes..0...No. <- as.factor(churn$Churn..1...Yes..0...No.)
churn_balanced_both <- ovun.sample(Churn..1...Yes..0...No. ~ ., data = TrainData, method = "both", p=0.5,N=4472, seed = 1)$data

table(churn_balanced_both$Churn..1...Yes..0...No.)

maxs = apply(churn_balanced_both[ , c(2, 4:13)], 2, max)
mins = apply(churn_balanced_both[ , c(2, 4:13)], 2, min)
scaled.data = as.data.frame(scale(churn_balanced_both[ , c(2, 4:13)], center = mins, scale = maxs - mins))
churn_balanced_both = data.frame(scaled.data,churn_balanced_both$Churn..1...Yes..0...No.)
churn_balanced_both = rename(churn_balanced_both,c("churn_balanced_both.Churn..1...Yes..0...No."="rate"))
null = glm(rate ~ Customer.Age..in.months., data= TrainData, family = "binomial") # only includes one variable 
full = glm(rate ~., data= TrainData, family = "binomial") # includes all the variables > # We can perform forward selection using the command: 
#step(null, scope=list(lower=null, upper=full), direction="forward")
#step(full, data=TrainData, direction="backward")  
step(null, scope = list(upper=full), data=TrainData, direction="both")

#KNN
View(chu)

balancechu<- ovun.sample(Churn..1...Yes..0...No. ~ ., data = chu, method = "both", p=0.5,N=6347, seed = 1)$data

maxs = apply(balancechu[ , c(2, 4:13)], 2, max)
mins = apply(balancechu[ , c(2, 4:13)], 2, min)
scaled.data = as.data.frame(scale(balancechu[ , c(2, 4:13)], center = mins, scale = maxs - mins))

churn = data.frame(scaled.data,balancechu$Churn..1...Yes..0...No.)
churn = rename(churn,c("balancechu.Churn..1...Yes..0...No."="result"))


set.seed(999)
ind = sample(2, nrow(churn), replace=TRUE, prob=c(0.7, 0.3))
churn.training = churn[ind==1,1:11]
churn.test = churn[ind==2, 1:11]

churn.trainLabels = churn[ind==1, 12]
churn.testLabels = churn[ind==2, 12] 




KNN_pred = knn(train = churn.training, test = churn.test, cl=churn.trainLabels, k=7)

KNN_pred
acc=table(KNN_pred,churn.testLabels)
accuracy<-sum(diag(acc))/sum(acc)
accuracy

View(traind)


model <- train(result ~ . , data=traind, method = 'knn', tuneGrid = expand.grid(.k=1:25),metric = 'Accuracy', trControl = trainControl(method = 'repeatedcv', number = 10, repeats = 15))
model
plot(model)
confusionMatrix(model)





