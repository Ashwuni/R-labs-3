---
title: "Assignment 3"
author: "Qirui Liu, Ashwuni Kumar, Gunchica Bhalla"
date: "27/03/2021"
geometry: margin=.75in
output:
  html_document:
    df_print: paged
    theme: cosmo
  word_document: default
  pdf_document: default
header-includes:
- \usepackage{graphicx}
- \usepackage{color}
graphics: yes
fontsize: 11pt
---

<!-- You can check templates from this website and change html theme: https://www.datadreaming.org/post/r-markdown-theme-gallery/ -->
<!-- It won't affect the PDF or Word. -->
1.1
```{r}
knitr::opts_chunk$set(message=FALSE, warning=FALSE)
library('tidyverse')
library('randomForest')
library('ISLR')
library('e1071')
library('rpart')
library('parsnip')
library('gridExtra')
library("MLmetrics")
library("caret")


croissant <- read.csv('croissant.csv')
circles <- read.csv('circles.csv')
varied <- read.csv('varied.csv')

croissant$y <- as.factor(croissant$y)
circles$y <- as.factor(circles$y)
varied$y <- as.factor(varied$y)

ggplot(croissant, aes(x=x1, y=x2, colour=y)) +geom_point()+ ggtitle("croissant")
ggplot(circles, aes(x=x1, y=x2, colour=y)) +geom_point()+ ggtitle("circles")
ggplot(varied, aes(x=x1, y=x2, colour=y)) +geom_point()+ ggtitle("varied")


```

1.2
```{r}
#crossiant
set.seed(112) # Set seed is needed if we want 
# to get the same random numbers always
train_size1 <- floor(0.5 * nrow(croissant))
train_inds1 <- sample(1:nrow(croissant), size = train_size1)
test_inds1  <- setdiff(1:nrow(croissant), train_inds1)

train1 <- croissant[ train_inds1 , ] 
test1  <- croissant[ test_inds1 , ]

cat('train size crossant:', nrow(train1), '\ntest size crossant:', nrow(test1))


#circles
set.seed(112) # Set seed is needed if we want 
# to get the same random numbers always
train_size2 <- floor(0.5 * nrow(circles))
train_inds2 <- sample(1:nrow(circles), size = train_size2)
test_inds2  <- setdiff(1:nrow(circles), train_inds2)

train2 <- circles[ train_inds2 , ] 
test2  <- circles[ test_inds2 , ]

cat('train size circles:', nrow(train2), '\ntest size circles:', nrow(test2))

#varied
set.seed(112) # Set seed is needed if we want 
# to get the same random numbers always
train_size3 <- floor(0.5 * nrow(varied))
train_inds3 <- sample(1:nrow(varied), size = train_size3)
test_inds3  <- setdiff(1:nrow(varied), train_inds3)

train3 <- varied[ train_inds3 , ] 
test3  <- varied[ test_inds3 , ]

cat('train size varied:', nrow(train3), '\ntest size varied:', nrow(test3))


```


1.3.1 croissant 
```{r}
#croissant logistic 
y.train1 <- train1$y
x.train1 <- model.matrix(y ~ .,train1)[,-1]
x.test  <- model.matrix(y ~ .,test1)[,-1]
fit1 <- glm(y ~ .,data= train1, family="binomial")
pred1 <- predict(fit1,test,type = "response") > 0.5
mean(pred1 == (test1$y==1))

#croissant decision tree 
library("tree")
croissant.tree <- tree(y ~ x1 + x2, data=train1)
plot(croissant.tree)
text(croissant.tree)

#crossiant svm
svmcro <-  svm(y ~ x1+x2, data=train1 , kernel ="linear", cost =0.05,scale =FALSE)

```


1.3.2 Plot
```{r}
preds.croissant.tree <- predict(croissant.tree, test1, type = "class")
preds.croissant.svm <- predict(svmcro,test1, type="class")


g1 <- ggplot(test1, aes(x1,x2,colour=y)) +
  geom_point() + 
  ggtitle("True Classes") + 
  theme(legend.position = "none")
g2 <- ggplot(test1, aes(x1,x2,colour=pred1)) +
  geom_point() + 
  ggtitle("Logreg Preds") + 
  theme(legend.position = "none")
g3 <- ggplot(test1, aes(x1,x2,colour=preds.croissant.tree)) +
  geom_point() + 
  ggtitle("Decision Tree Preds") + 
  theme(legend.position = "none")
g4 <- ggplot(test1, aes(x1,x2,colour=preds.croissant.svm)) +
  geom_point()+
  ggtitle("svm") + 
  theme(legend.position = "none")
grid.arrange(g1,g2,g3,g4,ncol=3)
```


1.3.3 Crossiant
```{r}
#Accuracy of logreg
mean(pred1 == (test1$y==1))
acc1    <- Accuracy(pred1, test1$y == 1)
acc1

preds.croissant.tree <- predict(croissant.tree, test1,type = "class")
ConfusionMatrix(preds.croissant.tree, test1$y)
acc2    <- Accuracy(preds.croissant.tree, test1$y)
acc2

preds.croissant.svm <- predict(svmcro,test1, type="class")
ConfusionMatrix(preds.croissant.svm, test1$y)
acc3    <- Accuracy(preds.croissant.svm, test1$y)
acc3
```
For croissant data, decision tree has the highest accuracy, decision tree is also less bias (one false positive/neg in total).



1.3 For circles

1.3.1 Circles
```{r}
#circles logistic 
y.train2 <- train2$y
x.train2 <- model.matrix(y ~ .,train2)[,-1]
x.test2  <- model.matrix(y ~ .,test2)[,-1]
fit2 <- glm(y ~ .,data= train2, family="binomial")
pred2 <- predict(fit2,test2,type = "response") > 0.5
mean(pred2 == (test2$y==1))

#circles decision tree 
library("tree")
circles.tree <- tree(y ~ x1 + x2, data=train2)
plot(circles.tree)
text(circles.tree)

#circles svm
svmcir <-svm(y~., data=train2, kernel="radial", cost = 10, gamma = 1)

```

1.3.2 Circles

```{r}
preds.circles.tree <- predict(circles.tree, test2, type = "class")
preds.circles.svm <- predict(svmcir,test2)


g1 <- ggplot(test2, aes(x1,x2,colour=y)) +
  geom_point() + 
  ggtitle("True Classes") + 
  theme(legend.position = "none")
g2 <- ggplot(test2, aes(x1,x2,colour=pred2)) +
  geom_point() + 
  ggtitle("Logreg Preds") + 
  theme(legend.position = "none")
g3 <- ggplot(test2, aes(x1,x2,colour=preds.circles.tree)) +
  geom_point() + 
  ggtitle("Decision Tree Preds") + 
  theme(legend.position = "none")
g4 <- ggplot(test2, aes(x1,x2,colour=preds.circles.svm)) +
  geom_point()+
  ggtitle("svm") + 
  theme(legend.position = "none")


grid.arrange(g1,g2,g3,g4,ncol=3)
```


1.3.3 Circles

```{r}
#Accuracy of logreg
acc1    <- Accuracy(pred2, test2$y == 1)
acc1

preds.circles.tree <- predict(circles.tree, test2,type = "class")
ConfusionMatrix(preds.circles.tree, test2$y)
acc2    <- Accuracy(preds.circles.tree, test2$y)
acc2

preds.circles.svm <- predict(svmcir,test2, type="class")
ConfusionMatrix(preds.circles.svm, test2$y)
acc3    <- Accuracy(preds.circles.svm, test2$y)
acc3
```
For circles data, svm has the highest accuracy and svm is less biased.




1.3 For Varied 

1.3.1 Varied
```{r}

#Varied decision tree 
library("tree")
varied.tree <- tree(y ~ x1 + x2, data=train3)
plot(varied.tree)
text(varied.tree)

#varied svm
svmvar <-  svm(y ~ x1+x2, data=train3 , kernel ="linear", cost =0.05,scale =FALSE)
```

1.3.2 varied

```{r}
preds.varied.tree <- predict(varied.tree, test3, type = "class")
preds.varied.svm <- predict(svmvar,test3, type="class")


g1 <- ggplot(test3, aes(x1,x2,colour=y)) +
  geom_point() + 
  ggtitle("True Classes") + 
  theme(legend.position = "none")

g3 <- ggplot(test3, aes(x1,x2,colour=preds.varied.tree)) +
  geom_point() + 
  ggtitle("Decision Tree Preds") + 
  theme(legend.position = "none")
g4 <- ggplot(test3, aes(x1,x2,colour=preds.varied.svm)) +
  geom_point()+
  ggtitle("svm") + 
  theme(legend.position = "none")


grid.arrange(g1,g3,g4,ncol=3)
```

1.3.3 varied
```{r}

preds.varied.tree <- predict(varied.tree, test3,type = "class")
ConfusionMatrix(preds.varied.tree, test3$y)
acc2    <- Accuracy(preds.varied.tree, test3$y)
acc2

preds.varied.svm <- predict(svmvar,test3, type="class")
ConfusionMatrix(preds.varied.svm, test3$y)
acc3    <- Accuracy(preds.varied.svm, test3$y)
acc3


```
For varied data, decision tree has the highest accuracy and decision tree is less biased (2 false pos,neg in total).


1.4 croissant validation
```{r}
set.seed(112)
ctrl <- trainControl(method="cv",number = 10)
fit1  <- train(form = y ~ x1+x2, data = train1, method = "glm", trControl = ctrl)
preds1 <- predict(fit1,newdata=test1)
mean(preds1 == (test1$y))

#croissant decision tree 
library("tree")
croissant.tree <- tree(y ~ x1 + x2, data=train1)
cv <- cv.tree(croissant.tree, FUN = prune.misclass)
ggplot() + 
  geom_point(aes(x=cv$size, y=cv$dev)) + 
  geom_line(aes(x=cv$size, y=cv$dev))
croissant.tree.pruned <- prune.misclass(croissant.tree,best=6)
plot(croissant.tree.pruned)
text(croissant.tree.pruned)
preds.croissant.tree.pruned <- predict(croissant.tree.pruned, test1,type = "class")

#crossiant svm
svmcro <-  svm(y ~ x1+x2, data=train1 , kernel ="linear", cost =0.05,scale =FALSE)
tune.out <- tune(svm, y ~ x1+x2, data=train1, kernel ="linear",
              ranges =list(cost=c(0.01, 0.05, .1 ,1 ,10 ,100 ,1000),
                           gamma=c(0.5,1,2,3,4)))

preds.svmCV <- predict(tune.out$best.model,test1)
```
1.4.2 crossiant graphs
```{r}
g1 <- ggplot(test1, aes(x1,x2,colour=y)) +
  geom_point() + 
  ggtitle("True Classes") + 
  theme(legend.position = "none")
g2 <- ggplot(test1, aes(x1,x2,colour=preds1)) +
  geom_point() + 
  ggtitle("Logreg Preds") + 
  theme(legend.position = "none")
g3 <- ggplot(test1, aes(x1,x2,colour=preds.croissant.tree.pruned)) +
  geom_point() + 
  ggtitle("Decision Tree Preds") + 
  theme(legend.position = "none")

g4 <- ggplot(test1, aes(x1,x2,colour=preds.svmCV)) +
  geom_point() + 
  ggtitle("SVm") + 
  theme(legend.position = "none")

grid.arrange(g1,g2,g3,g4,ncol=3)
```


1.4.3 crossiant accuracy
```{r}
acc1    <- Accuracy(preds1, test1$y)
acc1

ConfusionMatrix(preds.croissant.tree.pruned, test1$y)
acc2    <- Accuracy(preds.croissant.tree.pruned, test1$y)
acc2

ConfusionMatrix(preds.svmCV, test1$y)
acc3    <- Accuracy(preds.svmCV, test1$y)
acc3
```
After CV, the decision tree seems to be the most accurate, and also seems to be less biased (1 false pos,neg in total).


1.4 circles validation
```{r}
set.seed(112)
ctrl <- trainControl(method="cv",number = 10)
fit2  <- train(form = y ~ x1+x2, data = train2, method = "glm", trControl = ctrl)
preds2 <- predict(fit2,newdata=test2)
mean(preds2 == (test2$y))

#circles decision tree 
library("tree")
circles.tree <- tree(y ~ x1 + x2, data=train2)
cv <- cv.tree(circles.tree, FUN = prune.misclass)
ggplot() + 
  geom_point(aes(x=cv$size, y=cv$dev)) + 
  geom_line(aes(x=cv$size, y=cv$dev))
circles.tree.pruned <- prune.misclass(circles.tree,best=15)
plot(circles.tree.pruned)
text(circles.tree.pruned)
preds.circles.tree.pruned <- predict(circles.tree.pruned, test2,type = "class")

#circles svm
svmcro <-  svm(y ~ x1+x2, data=train2 , kernel ="radial", cost =0.05,scale =FALSE)
tune.out <- tune(svm, y ~ x1+x2, data=train2, kernel ="radial",
              ranges =list(cost=c(0.01, 0.05, .1 ,1 ,10 ,100 ,1000),
                           gamma=c(0.5,1,2,3,4)))

preds.svmCV <- predict(tune.out$best.model,test2)
```

1.4.2 circles graphs
```{r}
g1 <- ggplot(test2, aes(x1,x2,colour=y)) +
  geom_point() + 
  ggtitle("True Classes") + 
  theme(legend.position = "none")
g2 <- ggplot(test2, aes(x1,x2,colour=preds2)) +
  geom_point() + 
  ggtitle("Logreg Preds") + 
  theme(legend.position = "none")
g3 <- ggplot(test2, aes(x1,x2,colour=preds.circles.tree.pruned)) +
  geom_point() + 
  ggtitle("Decision Tree Preds") + 
  theme(legend.position = "none")

g4 <- ggplot(test2, aes(x1,x2,colour=preds.svmCV)) +
  geom_point() + 
  ggtitle("SVm") + 
  theme(legend.position = "none")

grid.arrange(g1,g2,g3,g4,ncol=3)
```

1.4.3 circles accuracy
```{r}
acc1    <- Accuracy(preds2, test2$y)
acc1

ConfusionMatrix(preds.circles.tree.pruned, test2$y)
acc2    <- Accuracy(preds.circles.tree.pruned, test2$y)
acc2

ConfusionMatrix(preds.svmCV, test2$y)
acc3    <- Accuracy(preds.svmCV, test2$y)
acc3
```
After CV, the svm seems to be the most accurate and also less biased (13 falae pos,neg in total).


1.4 varied validation
```{r}
set.seed(112)


#varied decision tree 
library("tree")
varied.tree <- tree(y ~ x1 + x2, data=train3)
cv <- cv.tree(varied.tree, FUN = prune.misclass)
ggplot() + 
  geom_point(aes(x=cv$size, y=cv$dev)) + 
  geom_line(aes(x=cv$size, y=cv$dev))
varied.tree.pruned <- prune.misclass(varied.tree,best=3)
plot(varied.tree.pruned)
text(varied.tree.pruned)
preds.varied.tree.pruned <- predict(varied.tree.pruned, test3,type = "class")

#varied svm
svmcro <-  svm(y ~ x1+x2, data=train3 , kernel ="linear", cost =0.05,scale =FALSE)
tune.out <- tune(svm, y ~ x1+x2, data=train3, kernel ="linear",
              ranges =list(cost=c(0.01, 0.05, .1 ,1 ,10 ,100 ,1000),
                           gamma=c(0.5,1,2,3,4)))

preds.svmCV <- predict(tune.out$best.model,test3)
```


1.4.2 varied graphs
```{r}
g1 <- ggplot(test3, aes(x1,x2,colour=y)) +
  geom_point() + 
  ggtitle("True Classes") + 
  theme(legend.position = "none")

g3 <- ggplot(test3, aes(x1,x2,colour=preds.varied.tree.pruned)) +
  geom_point() + 
  ggtitle("Decision Tree Preds") + 
  theme(legend.position = "none")

g4 <- ggplot(test3, aes(x1,x2,colour=preds.svmCV)) +
  geom_point() + 
  ggtitle("SVm") + 
  theme(legend.position = "none")

grid.arrange(g1,g3,g4,ncol=3)
```

1.4.3 varied accuracy
```{r}

ConfusionMatrix(preds.varied.tree.pruned, test3$y)
acc2    <- Accuracy(preds.varied.tree.pruned, test3$y)
acc2

ConfusionMatrix(preds.svmCV, test3$y)
acc3    <- Accuracy(preds.svmCV, test3$y)
acc3
```

For vaired with CV, the results are very close, but decision tree has slightly better accuracy, and seems to be less biased as well (2 false positive and negatives in total).
