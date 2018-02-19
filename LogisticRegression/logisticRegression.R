sigmoid <- function(z) {
  g = 1/(1+exp(-z))
  return(g)
}

costFunctionLogReg <- function(x, y, theta) {
  z = x%*%theta
  h = sigmoid(z)
  z1 = as.numeric(-(t(y) %*% log(h)))
  z2 = as.numeric(-(t(1-y) %*% log(1-h)))
  J = (1/length(y))*(z1 + z2)
  return(J)
}

gradientDescentLogReg <- function(x, y, theta, alpha, max.iters) {
  J_list = c()
  theta_list = c()
  for(i in 1:max.iters) {
    temp = matrix(0,ncol(x),1)
    z = x%*%theta
    h = sigmoid(z)
    for(j in 1:length(theta)) {
      temp[j] = theta[j] - (alpha/nrow(x))*t(h-y)%*%as.matrix(x[,j])
    }
    J_list[i] = costFunctionLogReg(x, y, temp)
    theta = temp
  }
  var_names = colnames(x)
  rownames(theta) = c("Intercept", var_names)
  return(list(J_list = J_list, theta = theta, theta_list = theta_list))
}