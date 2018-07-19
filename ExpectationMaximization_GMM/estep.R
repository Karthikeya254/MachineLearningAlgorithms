library(mvtnorm)
library(MASS)
source("utils.R")

#Expectation Step
Estep <- function(data, num_clusters, mu, sigma, prior) {
  w = matrix(0,nrow(data),num_clusters) #(n x k) matrix
  for(j in 1:nrow(data)) {
    for(i in 1:num_clusters) {
      
      #===================================================
      # Implementation of density function
      # mX = as.matrix(data[j,] - mu[i,]) #column vector
      # if(is.not.singular(sigma[[i]])) {
      #   s = t(mX) %*% solve(sigma[[i]]) %*%  mX
      #   f = ((1/(2*pi))^(ncol(data)/2)) * (1/sqrt(det(sigma[[i]]))) * exp((-1/2)*s)
      # } else {
      #   s = t(mX) %*% ginv(sigma[[i]]) %*%  mX
      #   dt = 1/(det(ginv(sigma[[i]])))
      #   f = ((1/(2*pi))^(ncol(data)/2)) * (1/sqrt(abs(dt))) * exp((-1/2)*s)
      # }
      #===================================================
      
      #density function package
      f = dmvnorm(data[j,], mu[i,], sigma[[i]])
      w[j, i] = f*prior[i]
    }
    w[j,] = w[j,]/sum(w[j,])
  }
  return(w)
}