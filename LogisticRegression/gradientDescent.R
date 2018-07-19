source("sigmoid.R")
source("costFunction.R")

gradientDescent <- function(x, y, theta, alpha, max.iters) {
  J_list = c()
  theta_list = c()
  for(i in 1:max.iters) {
    temp = matrix(0,ncol(x),1)
    z = x%*%theta
    h = sigmoid(z)
    for(j in 1:length(theta)) {
      temp[j] = theta[j] - (alpha/nrow(x))*t(h-y)%*%as.matrix(x[,j])
    }
    J_list[i] = costFunction(x, y, temp)
    theta = temp
  }
  var_names = colnames(x)
  rownames(theta) = c("Intercept", var_names)
  return(list(J_list = J_list, theta = theta, theta_list = theta_list))
}