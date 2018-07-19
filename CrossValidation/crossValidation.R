require(caret)

# creates k folds with ids of dataset
createKFolds <- function(cls_vec, k) {
  cls_values_unique = unique(cls_vec)
  folds = vector("list", length(cls_values_unique))
  for(i in 1:length(cls_values_unique)) {
    cls_ids = which(cls_vec == cls_values_unique[i])
    id = split(sample(cls_ids), sample(1:k))
    folds = mapply(c, folds, id, SIMPLIFY=FALSE)
  }
  return(folds)
}

# creates the list of K training and test data ids 
createIdsCV <- function(data, cls_var, k) {
  kfolds = createKFolds(data[,cls_var], k)
  cv_ids = list()
  for(i in 1:k) {
    train_ids = setdiff(unlist(kfolds), kfolds[[i]])
    test_ids = kfolds[[i]]
    cv_ids[[i]] = train_ids
    cv_ids[[i+k]] = test_ids
  }
  return(cv_ids)
}