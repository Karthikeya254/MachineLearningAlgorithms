source(costFunction)

normalEquation <- function(x, y) {
  theta = solve(t(x) %*% x) %*% t(x) %*% y
  var_names = colnames(x)
  rownames(theta) = c("Intercept", var_names)
  return(theta)
}