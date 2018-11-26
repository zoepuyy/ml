
setwd("D:/18LSE/Courses/ST443 ML/project/rcode")
source('coordinate_descent_lasso.R')
source('crossval_lasso.R')
library(ISLR)


Hitters=na.omit(Hitters)
y=Hitters$Salary
X=data.matrix(Hitters[, -which(names(Hitters)=='Salary')])

##########lasso ##########

cv_lasso=cv.lasso(y=y,X=X,lambda_max=5,step_lambda = 0.2)
plot_cv_lasso(cv_lasso)

#compare with the glmnet
library('glmnet')
HX <- scale(X)
Hy <- y - mean(y)
fit.ridge <-glmnet(HX, Hy, alpha=1) 
cv <-cv.glmnet (HX, Hy, alpha=1)
plot(cv)
cv$lambda.min
cv$lambda.1se
#different many times, seed?

fit.ridge1 <-glmnet(X, y, alpha=1) 
cv1 <-cv.glmnet (X, y, alpha=1)
plot(cv1)

#####elastic net######
source('coordinate_descent_EN.R')
a=elasticNet.solve(y=y,X=X,lambda=3,alpha=0.8)
b=lasso.solve(y=y, X=X, lambda=3)


source('crossval_EN.R')
EN_min = cv.EN(y = y, X = X, lambda_max = 5, step_lambda = 0.5, one_stderr_rule = FALSE)
EN_1se = cv.EN(y = y, X = X, lambda_max = 5, step_lambda = 0.5)
plot_cv_EN(EN_1se)


# 1 what if it¡¯s not convergent within 500 times?
# 2 take much time?(or for my computer only)
# 3 the plot?
# 4 what is the proper lambda_max.(eg. for Hitters)best lambda always the max¡­
