source("estep.R")
source("mstep.R")
source("utils.R")

#Execute EM using this function
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
        cat("Zero weights...\n")
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
        cat("Singular Matrix...\n")
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