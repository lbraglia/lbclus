#' K-means with better defaults
#' 
#' @examples
#' data(clusterdata1)
#' set.seed(1)
#' km <- cluster_kmeans(clusterdata1, k=3)
#' plot_cluster(clusterdata1, km$cluster)
#' cluster_profiles(clusterdata1, km$cluster)
#' 
#'@export
cluster_kmeans <- function(x, k, nstart = 100, iter.max = 100)
  stats::kmeans(x, centers = k, nstart = nstart, iter.max = iter.max)



#' PAM
#'
#' K-means on distance to medoids (object)
#' @param d data or distance (if data euclidean distance is used)
#' @param k number of clusters
#' 
#' @examples
#' data(bundestag)
#' b <- bundestag[1:5]
#' d <- dist_manhattan(b)
#' set.seed(1)
#' p <- lbclus::pam(d=d, k=5)
#' 
#' @export
cluster_pam <- function(d, k) cluster::pam(x = d, k = k)


# ------------------------------- hierarchical clustering
  

#' Hierarchical clustering
#'
#' Average linkage by default other wise set method
#' If k is specified cutree is 
#' 
#' @examples
#' 
#' data(veronica)
#' # jaccard on veronica, average link
#' d <- dist_jaccard(veronica)
#' plot(cluster_hier(d)) # average linkage by default
#' cluster_hier(d, k=8)
#'
#' # euclidean on scaled geyser, average link
#' data(geyser)
#' d <- dist_eucl(scale(geyser))
#' hcl <- cluster_hier(d, k=4)
#' par(mfrow=c(1,2))
#' plot(hcl$hclus)
#' plot_cluster(scale(geyser), hcl$cluster)
#' 
#' @export
cluster_hier <- function(d, k = NULL,
                         method=c("average", "single", "complete", "ward.D2")){
  method <- match.arg(method)
  hcl <- stats::hclust(d, method = method)
  if (is.null(k))
    return(hcl)
  else
    return(list("hclust" = hcl, "cluster" = stats::cutree(hcl, k = k)))
}
