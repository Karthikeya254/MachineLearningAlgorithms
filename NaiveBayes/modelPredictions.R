#Get predicted class vector
getNBprediction <- function(test_data, model_params) {
  priors = model_params$priors
  probability_params = model_params$probability_params
  class_levels = model_params$class_levels
  pred = c()
  for(i in 1:nrow(test_data)) {
    x = test_data[i,]
    posterior_probs = list()
    for(cls in class_levels) {
      posterior = priors[cls]
      for(name in colnames(x)) {
        if(is.factor(x[,name])) {
          if(x[,name] %in% rownames(probability_params[[name]])) {
            posterior = posterior * probability_params[[name]][as.character(x[, name]), cls]  
          }else { #m-estimate (mostly shouldn't touch this area, but just to be on safe side)
            p = 1/(1+nrow(probability_params[[name]]))
            m = 1+nrow(probability_params[[name]])
            nj = priors[cls]*model_params$num_records
            estimated_prob = m*p/(nj + m)
            posterior = posterior * estimated_prob
          }
        }else {
          mu = probability_params[[name]]["mu", cls]
          sig = probability_params[[name]]["sig", cls]
          prob_dens = (1/sqrt(2*pi*(sig^2)))*exp(-((x[,name] - mu)^2)/(2*sig^2))
          posterior = posterior * prob_dens
        }
      } 
      posterior_probs[[cls]] = posterior
    }
    pred[i] = names(which.max(posterior_probs))
  }
  return(pred)
}

getPredictionError <- function(true_label, pred_label) {
  return(sum(pred_label != true_label)/length(true_label))
}