#' K-means with better defaults
#' 
#' @examples
#'
#' set.seed(1)
#' km <- lbclus::kmeans(clusterdata1, k=3)
#' cluster_plot(clusterdata1, km$cluster)
#' cluster_profiles(clusterdata1, km$cluster)
#' 
#'@export
kmeans <- function(x, k, nstart = 100, iter.max = 100)
  stats::kmeans(x, centers = k, nstart = nstart, iter.max = iter.max)



#' PAM
#'
#' K-means on distance to medoids (object)
#' @param d data or distance (if data euclidean distance is used)
#' @param k number of clusters
#' 
#' @examples
#'
#' b <- bundestag[1:5]
#' d <- dist_manhattan(bundestag)
#' set.seed(1)
#' lbclus::pam(d=d, k=5)
#' 
#' @export
pam <- function(d, k) cluster::pam(x = d, k = k)

