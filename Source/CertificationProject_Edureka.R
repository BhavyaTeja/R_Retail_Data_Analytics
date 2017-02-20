#Setting the working directory

setwd("G:\\Masters\\Data Analytics with R\\Project")

#Libraries

library(rpart)
library(ROCR)
library(pscl)

#Reading the data

data = read.csv("G:\\Masters\\Data Analytics with R\\Project\\Retail_Case_Study_Data.csv")

data = data.frame(data)

#Data Cleaning

data[data == ' ?'] = NA         

data = na.omit(data) 

data = data.frame(data)

#Data Exploratory Functions

head(data)
tail(data)
str(data)

summary(data)

#Customer is most likely to buy a product if he is a new customer in an urban area

data$Cust.Sales = 0
for ( i in 1:nrow(data))
{
  if(data[i, "New.Customer"] == 1)
  {
    if(data[i,"Area"] == "Urban")
    {
      data[i, "Cust.Sales"] = 1 
    }
  }
}

#Data Sampling 

GoodData_1 = data[data$Sale.Made == 1,]
GoodData_0 = data[data$Sale.Made == 0,]

round(nrow(GoodData_0)*0.8,0)
round(nrow(GoodData_1)*0.8,0)

TrainData_0 = GoodData_0[1:(round(nrow(GoodData_0)*0.8,0)),]
TestData_0 = GoodData_0[((round(nrow(GoodData_0)*0.8,0))+1):nrow(GoodData_0),]

TrainData_1 = GoodData_1[1:(round(nrow(GoodData_1)*0.8,0)),]
TestData_1 = GoodData_1[((round(nrow(GoodData_1)*0.8,0))+1):nrow(GoodData_1),]

Testdata = rbind(TestData_0, TestData_1)
Traindata = rbind(TrainData_0, TrainData_1)

table(Testdata$Sale.Made)
table(Traindata$Sale.Made)

#Buidling the model using Logistic Regression

data$Spend.Category = factor(data$Spend.Category)
data$Purchase.Channel = factor(data$Purchase.Channel)

Regmodel = glm(Sale.Made ~ Months.Since.Last.Buy + Spend.Numeric + Mens.Merchandise + Womens.Merchandise + New.Customer + Visited.Website + Cust.Sales, data = Traindata, family = binomial(logit))

summary(Regmodel)

Regmodel1 = glm(Sale.Made ~ Spend.Numeric + Visited.Website, data = Traindata, family = "binomial")

summary(Regmodel1)

#AIC Value of Regmodel1 is lower than the Regmodel, hence we use Regmodel1

#Predicting the model using the Test data

Regmodel1_Predict = predict(Regmodel1, Testdata, type = "response")
Regmodel1_Predict = data.frame(Regmodel1_Predict)
Regmodel1_Predict = round(Regmodel1_Predict,0)

Combineddata = cbind(Testdata$Sale.Made, Regmodel1_Predict)
colnames(Combineddata) = c("Actual", "Predicted")

#Validating the performance of the model using Confusion Matrix and ROCR Curve

table(Combineddata$Actual, Combineddata$Predicted)

Prediction_Reg = prediction(as.numeric(unlist(Regmodel1_Predict)), as.numeric(Testdata$Sale.Made), label.ordering = NULL)
Performance_Reg = performance(Prediction_Reg, "tpr", "fpr")
plot(Performance_Reg)