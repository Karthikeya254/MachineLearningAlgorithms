source("updateCentroids.R")
source("assignClusters.R")

executeLloydsKMeans <- function(data, num_clusters, centroids, num_iters, convergence) {
  for(i in 1:num_iters) {
    previous_centroids = centroids
    new_clusters = assignClusters(data, num_clusters, centroids)
    centroids = updateCentroids(data, num_clusters, new_clusters)
    conv = sum((previous_centroids - centroids)^2)
    if(conv <= convergence || i == num_iters) {
      return(list(iteration = i, final_centroids = centroids, final_clusters = new_clusters))
    }
  }
}