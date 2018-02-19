totWithinSS <- function(data, num_clusters, centroids, clusters) {
  sse = 0
  for(i in 1:num_clusters) {
    idx = which(clusters == i)
    data_i = data[idx, ]
    if(nrow(data_i)>0) {
      sse = sse + sum(rowSums((t(t(data_i) - centroids[i,]))^2))
    }else {
      warning("Empty Cluster found")
    }
  }
  return(sse)
}