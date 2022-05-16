---
title: "assignment 3"
author: "Qirui Liu, Ashwuni Kumar, Gunchica Bhalla"
geometry: margin=.75in
output:
  pdf_document: default
  word_document: default
  html_document:
    df_print: paged
    theme: cosmo
header-includes:
- \usepackage{graphicx}
- \usepackage{color}
graphics: yes
fontsize: 11pt
---
2.1
```{r}
knitr::opts_chunk$set(message=FALSE, warning=FALSE)
library('tidyverse')
library('ISLR')
library('readr')
hitters = Hitters
heart = read.csv('Heart.csv')

head(hitters)
head(heart)
```


2.1.1
```{r}

#  As per campus I am adding 2.1.2, 2.1.3 here:
hitters$Salary = log(hitters$Salary)



hitters = na.omit(hitters)
hitters$Salary = log(hitters$Salary)
set.seed(112) 

train_size_hitters <- floor(0.7 * nrow(hitters))
train_inds_hitters <- sample(1:nrow(hitters), size = train_size_hitters)
train_hitters <-  hitters[ train_inds_hitters , ] 
test_hitters <-  hitters[ -train_inds_hitters , ]

heart = select(heart,c('Age','Sex','ChestPain','RestBP','Chol','Fbs','RestECG','MaxHR','ExAng','Oldpeak','Slope','Ca','Thal','AHD'))
heart = na.omit(heart)
heart$AHD = as.factor(heart$AHD)
heart$Age = as.numeric(as.character(heart$Age))

train_size_heart <- floor(0.7 * nrow(heart))
train_inds_heart <- sample(1:nrow(heart), size = train_size_heart)
train_heart <- heart[ train_inds_heart , ] 
test_heart  <- heart[ -train_inds_heart , ]

cat("Hitters Data Set")
cat('\ntrain size:', nrow(train_hitters), '\ntest size:', nrow(test_hitters))

cat("\n\nHeart Data Set")
cat('\ntrain size:', nrow(train_heart), '\ntest size:', nrow(test_heart))

```

2.1.2
```{r}
# hitters$Salary = log(hitters$Salary)
```

2.1.3
```{r}

# heart = select(heart,c('Age','Sex','ChestPain','RestBP','Chol','Fbs','RestECG',
# 'MaxHR','ExAng','Oldpeak','Slope','Ca','Thal','AHD'))

```

2.2.1
```{r}
library(rattle)
library("tree")
library("rpart")
library("rpart.plot")
hitter.tree = rpart(Salary ~ Hits+Years,data = train_hitters)
```

2.2.2
```{r}

fancyRpartPlot(hitter.tree)

```

2.2.3
```{r}
exp(1.9)
```


2.2.4
```{r}
library(Metrics)
preds_hitters = predict(hitter.tree,test_hitters)
rmseTree = rmse(preds_hitters,test_hitters$Salary)
rmseTree
```

2.3.1
```{r}
library(MLmetrics)
heart.tree = rpart(AHD ~., data = train_heart)

```

2.3.2
```{r}
fancyRpartPlot(heart.tree,caption = "")
```



2.3.3
```{r}
test_heart_subset = test_heart[1:5,]

test_heart_subset$AHD = as.numeric(test_heart_subset$AHD)

preds_heart = predict(heart.tree, test_heart_subset)
rmseclasstree = RMSE(preds_heart, test_heart_subset$AHD)
rmseclasstree

```


2.3.4
```{r}

preds_confmat   <- predict(heart.tree, type = "class")
ConfusionMatrix(preds_confmat, train_heart$AHD)

```

2.4.1
```{r}
library(randomForest)
complete_set_hitters = complete.cases(train_hitters)
```

2.4.2
```{r}
hitters.bag = randomForest(Salary ~ .,data = train_hitters, mtry = ncol(train_hitters)-1)
```

2.4.3
```{r}
preds_hitters.bag = predict(hitters.bag,test_hitters)
head_preds = head(preds_hitters.bag,4)
cat("Andre Dawson - \t\tActual Salary: ", hitters[3,]$Salary, "\tPredicted Salary: " , 
    head(preds_hitters.bag,1))
cat("\nAndres Galarraga - \tActual Salary: ", hitters[5,]$Salary, "\tPredicted Salary: " , 
    preds_hitters.bag[2])
cat("\nAndres Thomas- \t\tActual Salary: ", hitters[8,]$Salary, "\tPredicted Salary: " ,
    preds_hitters.bag[3])
cat("\nAlex Trevino- \t\tActual Salary: ", hitters[11,]$Salary, "\tPredicted Salary: " , 
    preds_hitters.bag[4])
cat("\n\nAll Predicted Salaries: " , preds_hitters.bag)

```

2.4.4 and 2.4.5
```{r}
rmseBag = rmse(preds_hitters.bag,test_hitters$Salary)
cat ("Tree Regression: ", rmseTree, "\n\n Bag Regression: ",rmseBag)

```

Discussion: The RMSE value for Bag Regression is lower than the RMSE value for Tree Regression. 

2.5.1
```{r}
library(randomForest)
complete_set_hitters = complete.cases(train_heart)
```

2.5.2
```{r}
heart_forest = randomForest(AHD ~ .,data = train_heart, mtry = ncol(train_heart)-1)
```

2.5.3
```{r}
preds_heart.bag = predict(heart_forest,test_heart)
head_preds = head(preds_heart.bag,4)

cat("First - \tActual Result: ", heart[1,]$AHD, "\tPredicted Result: " , head(preds_heart.bag,1))
cat("\nSecond - \tActual Result: ", heart[2,]$AHD, "\tPredicted Result: " , head_preds[2])
cat("\nThird- \t\tActual Result: ", heart[3,]$AHD, "\tPredicted Result: " ,head_preds[3])
cat("\nFourth- \tActual Result: ", heart[4,]$AHD, "\tPredicted Result: " , head_preds[4])

cat("\n\nAll Predicted Results: " , preds_hitters.bag)
```

2.5.4
```{r}

ConfusionMatrix(preds_heart.bag, test_heart$AHD)

```

2.5.5
```{r}
# As per campuswire, can use rmse for accuracy
rmseClassBag = rmse(as.numeric(preds_heart.bag),as.numeric(test_heart$AHD))
cat ("Tree Classification: ", rmseclasstree, "\n\n Bag Classification: ",rmseClassBag)

```

Discussion: The RMSE value for Bag Regression is substantially lower than the RMSE value for Tree Regression.


2.6.1 and 2.6.2
```{r}

hitters.forest <- randomForest(Salary ~ . , data = train_hitters,mtry = 
                                 sqrt(ncol(train_hitters)-1) , importance=T) 
```

2.6.3
```{r}
preds_hitters.forest = predict(hitters.forest,test_hitters)

preds_hitters.forest[1:4]
```

2.6.4 and 2.6.5
```{r}
rmseForest = rmse(preds_hitters.forest,test_hitters$Salary)
cat ("Bag Regression: ", rmseBag, "\n\n Forest Regression: ",rmseForest)
```
Discussion: The RMSE value for Bag Regression is lower than the RMSE value for Tree Regression. 

