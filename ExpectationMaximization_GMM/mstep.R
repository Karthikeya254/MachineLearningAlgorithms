library(mvtnorm)
library(MASS)

#Maximization Step
Mstep <- function(data, num_clusters, w) {
  mu = matrix(0,num_clusters,ncol(data))
  sigma = replicate(num_clusters, matrix(0,ncol(data),ncol(data)), simplify = FALSE)
  pi = c()
  wt = t(w)%*%data #(k x d) => w is (n x k) and data is (n x d)
  cw = colSums(w) #(1xk)
  
  #Updating mean
  mu = sweep(wt, MARGIN = 1, cw, '/') #scalar multiplication of each data row vector with its corresponding weight
  
  for(i in 1:num_clusters) {
    
    #===================================================
    # Vectorized implementation of covariance update
    # mX = sweep(data, MARGIN = 2, mu[i,], '-') #(n x d) matrix
    # mX = as.matrix(data[j,] - mu[i,]) #column vector
    # wmX = sweep(t(mX), MARGIN = 2, w[, i], '*') #(d x n) matrix
    # sigma[[i]] = wmX %*% mX #(d x d) matrix
    # sigma[[i]] = sigma[[i]]/sum(w[, i])
    #===================================================
    
    #Updating covariance
    sigma[[i]] = cov.wt(data, wt = w[, i])$cov
    #Updating priors
    pi[i] = sum(w[, i])/nrow(data)
    
    #Adding some error to the diagonal of covariance matrix to make it invertible
    if(!(is.not.singular(sigma[[i]]))) {
      sigma[[i]] = sigma[[i]] + diag(0.0001, ncol(data))
    }
  }
  return(list(n_mu = mu, n_sigma = sigma, n_prior = pi))
}
