#Loading DataSet
library("readxl")
# xls files
titanic <- read_excel("D:/PGA/Exam/titanic.xls",col_names = TRUE)

#checking for the summary
summary(titanic)
ncol(titanic)
nrow(titanic)
colnames(titanic)

## Check data structure
str(titanic)


## Let's check for any missing values in the data
colSums(is.na(titanic))

#Check for the number of unique values in columns
sapply(titanic, function(x) length(unique(x)))

#Imputing missing values

#As we can see there are missing value in char var Embarked
#So to impute missing values in char var I will apply knn imputer
#This imputer is use to predict missing values

#Installing the VIM library
install.packages("VIM", dependencies = TRUE)

#loadig the library
library(VIM)
?kNN()

#Applying knn on the char data to predict mising values
titanic <- kNN(titanic, variable = "Embarked", k=5)

#Now we can see there is no NA in the Embarked column
#But knn imputer will add one more column in the dataset we will delete that column
summary(titanic)
colSums(is.na(titanic))

#Now we have missing values in numerical column i.e. Age and Cabin
#for that we need to check for the skewness in the Age column
#We can do so using histogram 
hist(titanic$Age)
#Now as per the Histogram data is right skeweed so we will impute median inplace of NAs
titanic$Age[is.na(titanic$Age)] <- median(titanic$Age,na.rm=T)

#we will again check for the NA
colSums(is.na(titanic))
nrow(titanic)

#We have only 891 rows and from that 891 rows Cabin has 687 missings.
#Because of the problem of too much missing data we will remove that column instead of imputing it.


## Removing Cabin as it has very high missing values, passengerId, Ticket and Name are not required
library(dplyr)
titanic <- titanic %>% select(-c(Cabin, PassengerId, Ticket, Name, Embarked_imp))

#Now For building a model we need to create dummies of categorical columns
str(titanic)

#We can create dummies of factor var so we need to convert var into factors

## Converting "Survived","Pclass","Sex","Embarked" to factors
for (i in c("Survived","Pclass","Sex","Embarked")){
  titanic[,i]=as.factor(titanic[,i])
}


## Create dummy variables for categorical variables
library(dummies)
titanic <- dummy.data.frame(titanic, names=c("Pclass","Sex","Embarked"), sep="_")


#checking the head 
head(titanic)


# create a list of 70% of the rows in the original dataset we can use for training

library(caret)
index <- createDataPartition(titanic$Survived, p=0.70, list=FALSE)

# use the remaining 70% of data to training and testing the models
train <- titanic[index,]

# select 30% of the data for validation
test <- titanic[-index,]


dim(train)
dim(test)

# Builging a Logistics Model
model <- glm(Survived ~ .,family=binomial(link='logit'),data=train)

# Model Summary
summary(model)

# Predicting Test Data
result <- predict(model,newdata=test,type='response')
result <- ifelse(result > 0.5,1,0)

table(result)
table(test$Survived)

# Getting Accuraccy in logistic model as Accuracy : 0.8045  i.e. 80%         
confusionMatrix(data=factor(result), reference=factor(test$Survived))

## ROC Curve and calculating the area under the curve(AUC)
library(ROCR)
predictions <- predict(model, newdata=test, type="response")
ROCRpred <- prediction(predictions, test$Survived)
ROCRperf <- performance(ROCRpred, measure = "tpr", x.measure = "fpr")

# Threshold  value of ROC curve shows us that on that point we will get best accuracy for the model
plot(ROCRperf, colorize = TRUE, text.adj = c(-0.2,1.7), print.cutoffs.at = seq(0,1,0.1))

auc <- performance(ROCRpred, measure = "auc")
auc <- auc@y.values[[1]]
auc


# Building Model in Knn 

#loading library for knn
library(class)

#Seprating X and Y of train and test
train_X = train[,2:12]
train_y = train[,1]

test_X = test[,2:12]
test_y = test[,1]

#checking for the length
lengths(train_X)
lengths(train_y)

#Buidling Model using Knn
knn_pred <- knn(train_X,test_X,train_y, k=3, prob=TRUE)


knn_pred
test_y
#Making Confusion Matrix uing Knn
#Got Accuracy : 0.7331  i.e. 73% using KNN
confusionMatrix(knn_pred,test_y)


#Building A Random Forest Model
library(randomForest)

# Create a Random Forest model with default parameters
rf_model <- randomForest(train$Survived ~ ., data = train, importance = TRUE)
rf_model

#Prediction on test data
pred <- predict(rf_model, test, type = "class")

# Checking classification accuracy
table(pred, test$Survived)  

#checking for the accuracy of the model
mean(pred == test$Survived)                    
# Getting Random Forest accuracy as 0.8458647 i.e. 84%

#Plotting the Error vs Number of Trees Graph.
plot(rf_model)

# To check important variables
importance(rf_model) 

# Ploting Important Variable
varImpPlot(rf_model) 

