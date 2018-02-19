updateCentroids <- function(data, num_clusters, clusters) {
  cents = matrix(0, nrow = num_clusters, ncol = ncol(data))
  for(i in 1:num_clusters) {
    index = which(clusters == i)
    if(length(index) == 1){
      cents[i,] = as.numeric(data[index, ])
    }else if(length(index) > 1){
      cents[i,] = (1/length(index))*colSums(data[index, ])
    }
  }
  return(cents)
}