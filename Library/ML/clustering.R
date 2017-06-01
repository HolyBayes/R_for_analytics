clusterize <- function(data, columns, clusters_cnt) {
  x <- data[, columns]
  clusters <- kmeans(x, clusters_cnt)
  return(clusters)
}