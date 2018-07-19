source(sigmoid)

costFunction <- function(x, y, theta) {
  z = x%*%theta
  h = sigmoid(z)
  z1 = as.numeric(-(t(y) %*% log(h)))
  z2 = as.numeric(-(t(1-y) %*% log(1-h)))
  J = (1/length(y))*(z1 + z2)
  return(J)
}