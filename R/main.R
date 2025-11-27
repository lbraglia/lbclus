#' @export
dist_eucl <- function(x) stats::dist(x=x, method = "euclidean")

#' @export
dist_minkowski <- function(x, p = 2) stats::dist(x = x, method="minkowski", p = p)

#' @export
dist_mahalanobis <- function(x) stats::dist(x = x, method="mahalanobis")

#' @export
dist_manhattan <- function(x) stats::dist(x = x, method = "manhattan")

#' @export
dist_smd <- function(x) nomclust::sm(x)
## dist_corr <- function(x) {}

#' @export
dist_gower <- function(x, type = list(ordinal = seq_len(ncol(x)))) cluster::daisy(x, metric = "gower", type = type)

#'@export
coords_mds <- function(x) smacof::mds(x)$conf

#'@export
coords_pca <- function(x, scale = FALSE){
  if (scale) x <- scale(x)
  pca <- stats::princomp(x)
  pca$scores[, 1:2]
}

#'@export
scale_mad <- function(x, center = TRUE, scale = TRUE){
  if (center) x <- x - median(x, na.rm=TRUE)
  if (scale)  x <- x / mad(x, na.rm = TRUE)
  x
}


#'@export
cluster_pch <- fpc::clusym  # choose pch

set.seed(1)

#'@export
cluster_col <- sample(grDevices::colors(), length(cluster_pch)) # choose colors

## function for plotting clusters

#'@export
plot_clus <- function(coords, clvec, ...){
  clustering_col <- cluster_col[clvec]
  clustering_pch <- cluster_pch[clvec]
  plot(coords, col = clustering_col, pch = clustering_pch, ...)
}

#'@export
kmeans <- function(x, k) kmeans(x, centers = k, nstart = 100, max.iter = 100)


#' @export
index_ari <- function(cl_x, cl_y) mclust::adjustedRandIndex(cl_x, cl_y)
