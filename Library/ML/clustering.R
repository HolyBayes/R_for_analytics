cluster <- function(data, first_feature, second_feature, clusters_cnt) {
  x <- data[,c(first_feature, second_feature)]
  clusters <- kmeans(x, clusters_cnt)
  return(clusters)
}