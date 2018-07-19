#Naive Bayes model param calculation
getModelParams <- function(train_data, cls_var) {
  class_labels = train_data[, cls_var]
  train_data = train_data[, setdiff(names(train_data), cls_var)] #removing class column
  priors = table(class_labels)/length(class_labels)
  class_levels = levels(class_labels) #unique classes
  probability_params = list()
  for (name in colnames(train_data)) {
    feat_vec = train_data[, name]
    if(is.factor(feat_vec)) { #for categorical variables
      prob_params = getProbForCat(feat_vec, class_labels)
    }else { #for continuous variables
      prob_params = getProbForCont(feat_vec, class_labels)
    }
    probability_params[[name]] = prob_params
  }
  return(list(priors = priors, probability_params = probability_params, class_levels = class_levels, num_records = nrow(train_data)))
}

#Computes probability parameters for categorical features
getProbForCat <- function(feat_vec, class_labels) {
  feat_levels = levels(feat_vec)
  class_levels = levels(class_labels)
  prob_params = matrix(0,length(feat_levels),length(class_levels),dimnames = list(feat_levels, class_levels))
  for(cls in class_levels) {
    #m-estimate check if any class doesn't have atleast one data record
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
  return(prob_params)
}

#Computes probability parameters for continuous features
getProbForCont <- function(feat_vec, class_labels) {
  class_levels = levels(class_labels)
  cont_levels = c("mu", "sig")
  prob_params = matrix(0,length(cont_levels),length(class_levels),dimnames = list(cont_levels, class_levels))
  for(cls in class_levels) {
    prob_params["mu", cls] = mean(feat_vec[class_labels == cls])
    prob_params["sig", cls] = sd(feat_vec[class_labels == cls])
  }
}