# 8.3 Lab: Decision Trees
## 8.3.1 Classification Trees
library(tree)
library(ISLR)
attach(Carseats)
names(Carseats)
High=ifelse(Sales<=8, 'No', 'Yes')
Carseats=data.frame(Carseats, High)
t1=tree(High~.-Sales, Carseats)
summary(t1)
plot(t1)
text(t1, pretty=0)
#t1
set.seed(2)
train=sample(1:nrow(Carseats), 200)
Carseats.test=Carseats[-train,]
t2=tree(High~.-Sales, Carseats, subset=train)
p2=predict(t2, Carseats.test, type='class')
High.test=High[-train]
table(p2, High.test)
# pruning: t2 may be overfitting to the training data
# pruning might help with improving test error rate, in
# addition to also increasing interpretability
set.seed(3)
t3=cv.tree(t2, FUN=prune.misclass) 
# cv.tree is used to choose optimal tree complexity
t3
par(mfrow=c(1,2))
plot(t3$size, t3$dev, type='b')
plot(t3$k, t3$dev, type='b')
# best model has 8 terminal nodes with 75% accuracy
t2.pruned=prune.misclass(t2, best=8)
par(mfrow=c(1,2))
plot(t2.pruned)
text(t2.pruned, pretty=0)
plot(t2)
text(t2)
# make prediction based on pruned tree
p2.pruned=predict(t2.pruned, Carseats.test, type='class')
table(p2.pruned, High.test) # gives 75.5% accuracy

## 8.3.2 Regression trees
library(MASS)
attach(Boston)
names(Boston)
set.seed(1)
train=sample(1:nrow(Boston), nrow(Boston)/2)
Boston.test=Boston[-train,]
tree.boston=tree(medv~., Boston, subset=train)
summary(tree.boston)
plot(tree.boston)
text(tree.boston, pretty=0)
# test RMSE
pred.boston=predict(tree.boston, Boston.test)
mean((pred.boston-Boston.test$medv)^2)
# use cv to choose the best tree
cv.tree.boston=cv.tree(tree.boston)
plot(cv.tree.boston$size, cv.tree.boston$dev, type='b')
#cv.pred.boston=predict(cv.tree.boston, Boston.test)
#mean((cv.pred.boston-Boston.test$medv)^2)

## 8.3.3 Bagging and Random Forests
library(randomForest)
set.seed(1)
bag.boston=randomForest(medv~., Boston, subset=train,
                        mtry=13, # use all 13 predictors i.e. bagging
                        importance=TRUE)
bag.boston
pred.bag=predict(bag.boston, Boston.test)
plot(pred.bag, Boston.test$medv)
abline(0, 1)
mean((pred.bag-Boston.test$medv)^2)
# train MSE=11.39; test MSE=23.59; overfitting much?
bag.boston=randomForest(medv~., Boston, subset=train,
                        mtry=13, # use all 13 predictors i.e. bagging
                        ntree=25, # 25 trees
                        importance=TRUE)
pred.bag=predict(bag.boston, Boston.test)
mean((pred.bag-Boston.test$medv)^2)
# rf
rf.boston=randomForest(medv~., Boston, subset=train,
                        mtry=6, # use all 13 predictors i.e. bagging
                        importance=TRUE)
rf.boston
pred.rf=predict(rf.boston, Boston.test)
plot(pred.rf, Boston.test$medv)
mean((pred.rf-Boston.test$medv)^2)

importance(rf.boston)
