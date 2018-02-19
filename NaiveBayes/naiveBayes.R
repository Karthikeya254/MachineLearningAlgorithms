#Naive Bayes model param calculation
getModelParams <- function(train_data, cls_var) {
  class_labels = train_data[, cls_var]
  train_data = train_data[, setdiff(names(train_data), cls_var)]
  priors = table(class_labels)/length(class_labels)
  class_levels = levels(class_labels)
  cont_levels = c("mu", "sig")
  probability_params = list()
  for (name in colnames(train_data)) {
    feat_vec = train_data[, name]
    if(is.factor(feat_vec)) { #for categorical variables
      feat_levels = levels(feat_vec)
      prob_params = matrix(0,length(feat_levels),length(class_levels),dimnames = list(feat_levels, class_levels))
      for(cls in class_levels) {
        #m-estimate check
        if(length(unique(feat_vec[class_labels == cls])) < length(feat_levels)) {
          cat("Using m-estimated probabilities for Feature=",name,"** for Class=",cls,"\n")
          p = 1/length(feat_levels)
          m = length(feat_levels)
          for(val in feat_levels) {
            prob_params[val, cls] = (length(which(feat_vec[class_labels == cls] == val)) + m*p)/(length(feat_vec[class_labels == cls]) + m)
          }  
        }else { #no need for m-estimate
          for(val in feat_levels) {
            prob_params[val, cls] = length(which(feat_vec[class_labels == cls] == val))/length(feat_vec[class_labels == cls])
          }
        }
      }
    }else { #for continuous variables
      prob_params = matrix(0,length(cont_levels),length(class_levels),dimnames = list(cont_levels, class_levels))
      for(cls in class_levels) {
        prob_params["mu", cls] = mean(feat_vec[class_labels == cls])
        prob_params["sig", cls] = sd(feat_vec[class_labels == cls])
      }
    }
    probability_params[[name]] = prob_params
  }
  return(list(priors = priors, probability_params = probability_params, class_levels = class_levels, num_records = nrow(train_data)))
}

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