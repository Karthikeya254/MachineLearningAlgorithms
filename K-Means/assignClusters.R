source("totalWithinSS.R")

# Assigns cluster labels to all the data points by computing closest(Euclidean Distance) cluster to each point
assignClusters <- function(data, num_clusters, centroids) {
  
  for(i in 1:num_clusters) {
    a = sqrt(rowSums((t(t(data) - centroids[i,]))^2))
    if(i==1){dist_mat = a}else{dist_mat = rbind(dist_mat, a)}
  }
  
  #coulmn of dist_mat is the distance of one data point to all the clusters
  assig_cl = apply(dist_mat,2,function(x) findUniqueMin(x))
  tie_idx = which(assig_cl==0)
  
  #Handling ties
  if(length(tie_idx)>0) {
    assig_cl = handleTies(data, num_clusters, centroids, assig_cl, tie_idx, dist_mat)
  }
  #Handling empty clusters
  assig_cl = handleEmptyClusters(data, num_clusters, assig_cl)
  
  return(assig_cl)
}

# Finds the cluster closest to a data point.
# It returns the cluster label if there is unique cluster closest to the point 
# It returns 0 if there are multiple clusters closest to the point
findUniqueMin <- function(x) {
  b = which(x == min(x))
  if(length(unique(b))==1){return(b)}else{return(0)}
}

# In case of tie the cluster label assignment that results in minimum "Total Within SS" is selected
handleTies <- function(data, num_clusters, centroids, clusters, tie_idx, dist_mat) {
  for(j in 1:length(tie_idx)) {
    #j is the index of data point that has tied clusters
    dist_vec = dist_mat[ , tie_idx[j]]#distance vector of data point j from all clusters
    idx = which(dist_vec == min(dist_vec))#all the tied clusters
    min_sse = 0
    for(l in 1:length(idx)) {
      q = idx[l]#one of the tied cluster
      clusters[j] = q
      tot_sse = totWithinSS(data, num_clusters, centroids, clusters)
      if(l==1){
        min_sse = tot_sse
        final_cl = q
      }else{
        if(tot_sse < min_sse) {
          min_sse = tot_sse
          final_cl = q
        }
      }
    }
    clusters[j] = final_cl
  }
  return(clusters)
}

# In case of an empty cluster, a random point is selected from data and assigned the corresponding cluster label
handleEmptyClusters <- function(data, num_clusters, clusters) {
  cluster_count = rep(1:num_clusters)
  unique_cls = unique(clusters)
  missed_cls = cluster_count[!(cluster_count %in% unique_cls)]
  updated_cls = clusters
  if(length(missed_cls)>0) {
    rndm = sample(nrow(data), length(missed_cls), replace = FALSE)
    for(i in 1:length(rndm)) {
      updated_cls[rndm[i]] = missed_cls[i]
    }
  }
  return(updated_cls)
}