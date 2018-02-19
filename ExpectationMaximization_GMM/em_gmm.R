library(mvtnorm)
library(MASS)

is.not.singular <- function(A) class(try(solve(A),silent=T))=="matrix"

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

Mstep <- function(data, num_clusters, w) {
  mu = matrix(0,num_clusters,ncol(data))
  sigma = replicate(num_clusters, matrix(0,ncol(data),ncol(data)), simplify = FALSE)
  sig = replicate(num_clusters, matrix(0,ncol(data),ncol(data)), simplify = FALSE)
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
      cat("Got Singular Matrix********************************************\n")
      cat("Sigma i::", i,"\n")
      sigma[[i]] = sigma[[i]] + diag(0.0001, ncol(data))
    }
  }
  return(list(n_mu = mu, n_sigma = sigma, n_prior = pi))
}


executeEM <- function(data, num_clusters, num_iters, init_mu, init_sigma, init_prior, convergence) {
  p_mu = init_mu
  p_sigma = init_sigma
  p_prior = init_prior
  old_likelihood = 0
  for(iter in 1:num_iters) {
    #Estep
    weight_matrix = Estep(data, num_clusters, p_mu, p_sigma, p_prior)
    
    #Check if any of the weight column is zero or NaN
    for(s in 1:num_clusters) {
      if(sum(weight_matrix[, s]) == 0 
         || any(is.infinite(weight_matrix[, s]) == TRUE)
         || any(is.nan(weight_matrix[, s]) == TRUE)) {
        cat("HIT Zero weights$$$$$$$$$$$$$$$$$$$$$$$")
        #Failed Run
        return(list(iterations = 0, weights = weight_matrix, f_mu = p_mu))
      }
    }
    
    #Mstep
    m_step = Mstep(data, num_clusters, weight_matrix)
    new_mu = m_step$n_mu
    new_sigma = m_step$n_sigma
    
    #check if the covariance matrix is still singular after adding error
    for(s in 1:length(new_sigma)) {
      if(!(is.not.singular(new_sigma[[s]]))) {
        cat("HIT a SINGULAR Matrix$$$$$$$$$$$$$$$$$$$$$$$")
        #Failed Run
        return(list(iterations = 0, weights = weight_matrix, f_mu = p_mu))
      }
    }
    
    #===================================================
    # Log likelihood convergence (to execute it uncomment this and comment the mean convergence below)
    # w1 = matrix(0,nrow(data),num_clusters) #(n x k) matrix
    # for(j in 1:nrow(data)) {
    #   for(i in 1:num_clusters) {
    #     f = dmvnorm(data[j,], new_mu[i,], new_sigma[[i]])
    #     w1[j, i] = f*p_prior[i]
    #   }
    # }
    # sw = rowSums(w1)
    # likelihood = sum(log(sw))
    # cat("log Likelihood::", likelihood, "\n")
    # l_conv = abs(likelihood - old_likelihood)
    # cat("log likelihood difference::", l_conv, "\n")
    # if(l_conv < 1e-3 || iter == num_iters) {
    #   return(list(iterations = iter, weights = weight_matrix, f_mu = p_mu))
    # }
    # old_likelihood = likelihood
    #===================================================
    
    #Difference between means of two iterations
    conv = sum((p_mu - new_mu)^2)
    
    #Mean convergence
    if(conv < convergence || iter == num_iters) {
      return(list(iterations = iter, weights = weight_matrix, f_mu = p_mu))
    }
    
    p_mu = new_mu
    p_sigma = new_sigma
    p_prior = m_step$n_prior
  }
}