costFunction <- function(x, y, theta) {
  h = x %*% theta
  J = (1/(2*length(y)))*sum((h-y)^2)
  return(J)
}

normalEquation <- function(x, y) {
  theta = solve(t(x) %*% x) %*% t(x) %*% y
  var_names = colnames(x)
  rownames(theta) = c("Intercept", var_names)
  return(theta)
}