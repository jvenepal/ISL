# chapter 6
# lab 1: subset selection
library(ISLR)
dim(Hitters)
sum(is.na(Hitters))
Hitters = na.omit(Hitters)
dim(Hitters)
library(leaps)
regfit.full = regsubsets(Salary~., Hitters, nvmax = 19)
regfit.summ = summary(regfit.full)
names(regfit.summ)
regfit.summ
par(mfrow=c(2,2))
# r2
max.r2 = which.max(regfit.summ$rsq)
plot(regfit.summ$rsq, xlab='Number of predictors',
     type='l')
points(max.r2, regfit.summ$rsq[max.r2],
       col='red', cex=2, pch=20)
# adj r2
plot(regfit.summ$adjr2, xlab='Number of predictors',
     type='l')
max.adjr2 = which.max(regfit.summ$adjr2)
points(max.adjr2, regfit.summ$adjr2[max.adjr2],
       col='red', cex=2, pch=20)
# cp
plot(regfit.summ$cp, xlab='Number of predictors',
     type='l')
min.cp = which.min(regfit.summ$cp)
points(min.cp, regfit.summ$cp[min.cp],
       col='red', cex=2, pch=20)
# bic
plot(regfit.summ$bic, xlab='Number of predictors',
     type='l')
min.bic = which.min(regfit.summ$bic)
points(min.bic, regfit.summ$bic[min.bic],
       col='red', cex=2, pch=20)
par(mfrow=c(2,2))
plot(regfit.full, scale="r2")
plot(regfit.full, scale="adjr2")
plot(regfit.full, scale="Cp")
plot(regfit.full, scale="bic")

# forward and backward stepwise selection
reg.fwd = regsubsets(Salary~., Hitters,
                     nvmax=19, method='forward')
summary(reg.fwd)
reg.bwd = regsubsets(Salary~., Hitters,
                     nvmax=19, method='backward')
summary(reg.bwd)
coef(reg.fwd, 7)
coef(reg.bwd, 7)
coef(regfit.full, 7)

# validation set approach and cross-validation
set.seed(10)
train=sample(c(TRUE,FALSE), nrow(Hitters), 
             replace=TRUE)
test = (!train)
regfit.best=regsubsets(Salary~., data=Hitters[train,],
                       nvmax=19)
test.mat=model.matrix(Salary~.,data=Hitters[test,])
val.error=rep(NA, 19)
for (i in 1:19){
  coeffs=coef(regfit.best, i)
  pred=test.mat[,names(coeffs)]%*%coeffs
  val.error[i]=mean((Hitters$Salary[test]-pred)^2)
}
val.error
nvars=which.min(val.error)
coef(regfit.best, which.min(val.error))
# refit on full data & choose the model with nvars variables
regfit.best=regsubsets(Salary~., data=Hitters,nvmax=19)
coef(regfit.best, nvars)

# crossvalidation
predict=function(object, testdata, id, ...) {
  form=object$call[[2]]
  test=model.matrix(form, data=testdata)
  coeffs=coef(object, id)
  test[,names(coeffs)]%*%coeffs
}
k=10 # 10-fold cv
set.seed(1)
folds=sample(1:k, nrow(Hitters), replace=TRUE)
cv.errors=matrix(NA, k, 19,dimnames=list(NULL, paste(1:19)))
for (j in 1:k){
  best.fit=regsubsets(Salary~., data=Hitters[folds!=j,], nvmax=19)
  for (i in 1:19){
    pred=predict(best.fit, Hitters[folds==j,], id=i)
    cv.errors[j,i]=mean((Hitters$Salary[folds==j]-pred)^2)
  }
}

