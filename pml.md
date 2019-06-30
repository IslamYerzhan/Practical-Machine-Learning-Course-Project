---
title: "Practical machine Learning course project"
output: html_document
---

Using devices such as Jawbone Up, Nike FuelBand, and Fitbit it is now possible to collect a large amount of data about personal activity relatively inexpensively. These type of devices are part of the quantified self movement – a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. One thing that people regularly do is quantify how much of a particular activity they do, but they rarely quantify how well they do it. In this project, your goal will be to use data from accelerometers on the belt, forearm, arm, and dumbell of 6 participants. They were asked to perform barbell lifts correctly and incorrectly in 5 different ways. More information is available from the website here: http://web.archive.org/web/20161224072740/http:/groupware.les.inf.puc-rio.br/har (see the section on the Weight Lifting Exercise Dataset).


##Load essential libraries

```{r, echo=TRUE,message=F,warning=FALSE}
library(caret)
library(rattle)
library(knitr)
library(rpart)
library(rpart.plot)
library(randomForest)
library(corrplot)
library(e1071)
```


##Load dataset
```{r, echo=TRUE,message=F,warning=FALSE}
train <- read.csv("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv",header = T)
test <- read.csv("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv",header = T)
dim(train)
dim(test)
str(train)
```


##Data cleaning

The training data set is consist of 19622 observations on 160 columns. We can notice that many columns have NA values or blank values on almost every observation. So we will remove them, because they will not produce any information. The first seven columns give information about the people who did the test, and also timestamps. We will not take them in our model.


Train dataset
```{r, echo=TRUE,message=F,warning=FALSE}
col_to_remove <- which(colSums(is.na(train) |train=="")>0.9*dim(train)[1]) 
Train_cleaned <- train[,-col_to_remove]
Train_cleaned <- Train_cleaned[,-c(1:7)]
dim(Train_cleaned)
```

Test dataset

```{r, echo=TRUE,message=F,warning=FALSE}
col_to_remove_ <- which(colSums(is.na(test) |test=="")>0.9*dim(test)[1]) 
Test_cleaned <- test[,-col_to_remove_]
Test_cleaned <- Test_cleaned[,-1]
dim(Test_cleaned)
```


Separate traintest to train and test 

```{r, echo=TRUE,message=F,warning=FALSE}
set.seed(123)
new_train1 <- createDataPartition(Train_cleaned$classe, p=0.75, list = F)
new_train <- Train_cleaned[new_train1,]
new_test <- Train_cleaned[-new_train1,]
```


In the following sections, we will test 3 different models :  classification tree, random forest, gradient boosting method
In order to limit the effects of overfitting, and improve the efficicency of the models, we will use the *cross-validation technique. We will use 5 folds.


##Decision tree
```{r, echo=TRUE,message=F,warning=FALSE}
Control <- trainControl(method = "cv", number = 5)
model1 <- train(classe~., data=new_train, method="rpart", trControl=Control)
```

Make plot
```{r, echo=TRUE,message=F,warning=FALSE}
fancyRpartPlot(model1$finalModel)
```

Make prediction
```{r, echo=TRUE,message=F,warning=FALSE}
model1_prediction <- predict(model1,newdata = new_test)
math1 <- confusionMatrix(new_test$classe,model1_prediction)
```

Accuracy
```{r, echo=TRUE,message=F,warning=FALSE}
math1$overall[1]
```

Model accuracy is 50%, not so good result. Еhe model will predict very weakly.



##Random forest


```{r, echo=TRUE,message=F,warning=FALSE}
model2 <- train(classe~., data=new_train, method="rf", trControl=Control, verbose=FALSE)
print(model2)
```

Make plot
```{r, echo=TRUE,message=F,warning=FALSE}
plot(model2)
```

Make prediction
```{r, echo=TRUE,message=F,warning=FALSE}
model2_prediction <- predict(model2,newdata=new_test)
math2 <- confusionMatrix(new_test$classe,model2_prediction)
```

Accuracy
```{r, echo=TRUE,message=F,warning=FALSE}
math2$overall[1]
```

Model accuracy is 99.5%, good result. 



#Gradient boosting

```{r, echo=TRUE,message=F,warning=FALSE}
model3 <- train(classe~., data=new_train, method="gbm", trControl=Control, verbose=FALSE)
```
Make a plot 

```{r, echo=TRUE,message=F,warning=FALSE}
plot(model3)
```


Make prediction

```{r, echo=TRUE,message=F,warning=FALSE}
model3_prediction <- predict(model3,newdata=new_test)
math3 <- confusionMatrix(new_test$classe,model3_prediction)
```

Accuracy

```{r, echo=TRUE,message=F,warning=FALSE}
math3$overall[1]
```

Accuracy is 96%, wery good result.


##Conclusion

Random forest model showed the best result. Our forecast will be based on the Random forest model.


```{r, echo=TRUE,message=F,warning=FALSE}
Ultimate_prediction <- predict(model2,newdata=Test_cleaned)
Ultimate_prediction
```

