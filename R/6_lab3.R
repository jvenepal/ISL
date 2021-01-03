library(pls)
Hitters=na.omit(Hitters)
dim(Hitters)
x=model.matrix(Salary~.,Hitters)[,-1] # converts cat. vars to dummy vars
y=Hitters$Salary
pcr.fit=pcr(Salary~.,data=Hitters, scale=TRUE, validation='CV')
summary(pcr.fit)
validationplot(pcr.fit, val.type='MSEP')
# train & test sets and CV
set.seed(1)
train=sample(c(TRUE, FALSE), size=nrow(Hitters), replace=TRUE)
test=(-train)
pcr.fit=pcr(Salary~.,data=Hitters,subset=train,validation='CV',scale=TRUE)
validationplot(pcr.fit, val.type='MSEP')
pcr.pred=predict(pcr.fit, x[test,], ncomp=7)
