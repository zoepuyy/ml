# In a function: simple version that works
# Assumes that X and y have already been properly standardized.
elasticNet.solve <- function(y, X, lambda = .01, alpha = .01, epsilon = .01){

  # rescale data
  X <- scale(X)
  y <- y - mean(y)

  # get p
  p <- ncol(X)

  # Init all betas = 0
  betas <- numeric(p)
  
  #Initiate the optimal funtion f with current betas
  #RSS is the Resisual Sum of Square
  #the combination penalty term =lambda*[(1-alpha)*l2+alpha*l1]
  #optimal funciton f=1/2n*RSS+penalty
  
  # RSS=t(y-x%*%betas)%*%(y-x%*%betas)
  # penalty=lambda*((1-alpha)*sum(x*x)+alpha*sum(Abs(betas)))
  # n=as.numeric (dim(y)[1])
  # f=RSS/(2*n)+penalty
  # 
  # Set hasConverged to False for now
  hasConverged <- FALSE
  
  #Keep track of the runs (useful for debugging)
  run <- 1

  while(!hasConverged){

    #print(run)

    betas_before <- betas # to keep track of convergence

    # update all betas until they converge

    for (j in 1:p){

      # force Bj = 0
      betas[j] = 0

      # get vector of partial residuals
      r_j <- (y - X %*% betas)

      # get OLS estimate of beta_j*
      # we can use cov because y and X are all standardized
      beta_star = cov(X[, j], r_j)

      # Update beta_j with soft_thresholding
      #lambda1=(1-alpha)*lambda
      #lambda2=alpha*lambda
      betas[j] <- sign(beta_star) * max(c((abs(beta_star) - lambda*alpha), 0)) /(1 + lambda*(1-alpha))

    }

    # # Get the new optimal funtion
    # RSS=t(y-x%*%betas)%*%(y-x%*%betas)
    # penalty=lambda*((1-alpha)*sum(x*x)+alpha*sum(Abs(betas)))
    # n=as.numeric (dim(y)[1])
    # f=RSS/(2*n)+penalty
    # 
    # diff_f=f-f_new
    # 
    # # check convergence, the difference of optimal functions is less thn elsilon
    # hasConverged <- (abs(diff_f)> epsilon) == 0

    ## not convergent! since little change in Betas results in huge change in f, it runs 70000times
    
    #if the both betas has the zeros in the same places, 
    #and the diff less than epsilon,then convergent
    Zeros=all((betas_before==0)==(betas==0))
    Diff_nonzeros=sum(abs(betas_before - betas) > epsilon) == 0
    hasConverged <- Zeros && Diff_nonzeros
    
    run <- run+1
    if(run==500){
      hasConverged=TRUE
      cat('beta for lambda=',lambda,'is not convergent within 500 times loop','\n')
    }
  

  }
  return(list('betas'=betas,'run'=run))
}
