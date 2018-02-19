source("executeLloydsKMeans.R")
source("totalWithinSS.R")

createElbowGraph <- function(data, min.k, max.k, max.iter, nstart = 1, convergence) {
  tot_ss = c()
  for(i in min.k:max.k) {
    tot_ss_run = c()
    for(run in 1:nstart) {
      init_centroids = data[sample(nrow(data),size=k,replace=FALSE),]
      km = executeLloydsKMeans(data, num_clusters = i, centroids = init_centroids, num_iters = max.iter, convergence = convergence)
      tot_ss_run[run] = totWithinSS(data, num_clusters = i, centroids = km$final_centroids, clusters = km$final_clusters)
    }
    tot_ss[i - min.k + 1] = mean(tot_ss_run)
  }
  k = rep(k.min:k.max)
  plot(k, tot_ss, type = "b", pch=20, xlab = "Number of Clusters", ylab = "Total within-cluster sum of squares")
  
}