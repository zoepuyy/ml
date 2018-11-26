#choose alpha,and lambda for elanstic net

setwd('D:/18LSE/Courses/ST443 ML/project/rcode')
source("coordinate_descent_EN.R")

# Clean Global Environment
library(ggplot2)
library(magrittr)
# rm(list = ls())

# Pick Lambda using cross-validation

cv.EN <- function(lambda_max = 10,
                     step_lambda = .1,
                     n_folds = 10,
                     y,
                     X,
                     one_stderr_rule = TRUE){
  
  # set proper K-folds
  if (nrow(X) %% n_folds != 0){
    while (nrow(X) %% n_folds != 0 ){ # not divisible by nfolds
      if(n_folds==4){
        force_k=TRUE
        break
      }
      n_folds <- n_folds - 1
    }
 
  if(!force_k)
    warning(paste0("The data cannot be split into the required numbers of folds. 
                   Using ", n_folds, " folds..."))
  else
    warning(paste("The data cannot be split into the required numbers of folds. 
                  The divisible K is too small,Using ", 
                  n_folds, " folds..."))
  }
  
  # randomly seprate the K groups
  folds <- sample(rep(1:n_folds, round(nrow(X) / n_folds)), size = length(y))
  
  # get MSE wrt lambda
  cv.one_passEN <- function(lambda, alpha){
    errors <- numeric(n_folds)
    for (i in 1:n_folds){
      
      Xtest = X[folds == i, ]
      ytest = y[folds == i]
      Xtrain = X[folds != i, ]
      ytrain = y[folds != i]}
      
      beta.EN <- elasticNet.solve(ytrain, Xtrain, lambda = lambda, alpha = alpha)
      ypred <- predict(beta.EN$betas, Xtest)
      
      errors[i] <- mean((ytest - ypred)^2)
      if(beta.EN$run==500){
        cat("when lambda=",lambda,'k=',i,'alpha=',alpha, '\n')
      }
    
    return(list(error = mean(errors), std = sd(errors)))
}

  
  lambdas <- seq(0, lambda_max, step_lambda)
  alphas<-seq(0,1,0.1)
  
  df <- data.frame(lambda = lambdas,
                   alpha = numeric(length(lambdas)),
                   estimated_error = numeric(length(lambdas)),
                   std = numeric(length(lambdas)))
  
  for (i in 1:length(df$lambda)){
    # for each lambda, choose the best alpha which gives min error
    # store the the data in alpha_best,error, std, always store the min one
    # initiate with alpha = 0
    alpha_best=0
    cv.EN <- cv.one_passEN(lambda = df$lambda[i],alpha = alpha_best)
    error=cv.EN$error
    std=cv.EN$std
    
    for ( j in 2:length(alphas)){
    cv.EN <- cv.one_passEN(lambda = df$lambda[i],alpha = alphas[j])
    
    if(error > cv.EN$error){
      error = cv.EN$error
      std = cv.EN$std
      alpha_best = alphas[j]
      }
    }
    #then put it in the df table, with the selected alpha and corresponding error and std

    df$estimated_error[i] <- error
    df$std[i] <- std
    df$alpha[i] <- alpha_best
  }
  
  #the confidence internval of MSE
  df$ci_up <- df$estimated_error + df$std
  df$ci_down <- df$estimated_error - df$std
  
  if (!one_stderr_rule){
    
    best_lambda <- df$lambda[which.min(df$estimated_error)]
    
  } else {
    
    thresh <- df$ci_up[which.min(df$estimated_error)]
    
    best_lambda <- max(df$lambda[df$estimated_error <= thresh])
    
  }
  
  df$is_best <- df$lambda == best_lambda
  
  cat("Best lambda is", best_lambda)
  
  return(list(errors = df, best_lambda = best_lambda))
  
}










##########
plot_cv_EN <- function(cv_results){
  
  dta <- cv_results$errors
  
  dta %>%
    ggplot(aes(x = lambda)) +
    geom_point(aes(y = estimated_error, color = is_best), size = 1, pch = 3) +
    geom_point(aes(y = ci_up), color = "red", size = 1, pch = 3) +
    geom_point(aes(y = ci_down), color = "red", size = 1, pch = 3) +
    geom_hline(yintercept = dta[which.min(dta$estimated_error), "ci_up"],
               color = "grey", size = .2) +
    scale_color_manual(name = "Best Lambda",
                       values = c("TRUE" = "green", "FALSE" = "black")) +
    geom_segment(aes(x=cv_results$best_lambda, xend=cv_results$best_lambda,
                     y=0, yend = dta[dta$is_best, "estimated_error"]),
                 lty = "dashed", size = .25, color = "green") +
    ylab("10-fold average cross-validation error")
  
}

# cv.lasso(lambda_max = 3, step_lambda  = .1, n_folds = 10, y = y, X = X, TRUE) %>%
#   plot_cv_lasso()
