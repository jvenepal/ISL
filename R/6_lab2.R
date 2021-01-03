# Ridge regression using glmnet

# glmnet doesn't accept formulas; it expects X matrix and y vector
x=model.matrix(Salary~.,Hitters)[,-1] # converts cat. vars to dummy vars
y=Hitters$Salary
dim(x)
library(glmnet)
grid=10^seq(10,-2,length=100)
rid.mod=glmnet(x,y,alpha=0,lambda=grid,
               standardize=TRUE) # automatically scales the predictors
dim(coef(rid.mod)) # 20 predictors, 100 lambdas
rid.mod$lambda[50]
coef(rid.mod)[,50] # coeff's corresponing to 50th lambda
sqrt(sum(coef(rid.mod)[-1,50]^2)) # l2-norm of 50th lambda
# ridge regression coefficients for lambda=50
predict(rid.mod,s=50,type="coefficients")[1:20,] # errors
# cv
set.seed(1)
train=sample(1:nrow(x),nrow(x)/2)
test=(-train)
rid.mod=glmnet(x[train,], y[train], alpha=0,
               lambda=grid, thresh=1e-12)
preds=predict(rid.mod, s=4, newx=x[test,]) # predict errors again!
