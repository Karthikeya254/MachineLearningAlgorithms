costFunction <- function(x, y, theta) {
  h = x %*% theta
  J = (1/(2*length(y)))*sum((h-y)^2)
  return(J)
}