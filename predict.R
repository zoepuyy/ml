# Predict Function
predict <- function(betas, new_data){
  return(as.matric(new_data)%*%as.matrix(betas))
}
