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

## -------------------------------------- MIXTURE

#' Gaussian mixture
#'
#' @examples
#' data(oliveoil)
#' olive <- oliveoil[, 3:10]
#' gmix <- cluster_gaussianmixture(olive, k=1:10, verbose=TRUE)
#' # here higher bic is better
#' table(gmix$classification)
#' @export
cluster_gaussianmixture <- function(data, k=1:10, verbose=TRUE, bic_plot=FALSE){
  cl <- mclust::Mclust(data, G=k)
  if (verbose) print(summary(cl))
  if (bic_plot) plot(cl, ask=FALSE, what="BIC")
  cl
}


# T-mixture e skew Gaussian se rimane tempo

#' Skew-T mixture
#' 
#' @param data data
#' @param k number of cluster to search for, optimized for BIC (lower better)
#' @param verbose show best result parameter estimates
#' 
#' @examples
#' data(oliveoil)
#' olive <- oliveoil[, 3:10]
#' solive <- scale(olive)
#' stolive <- cluster_skewtmixture(solive)
#' # here smaller BIC is better
#' 
#' @export
cluster_skewtmixture <- function(data, k=1:10, verbose=TRUE){
  cl <- MixtureMissing::MGHM(X=data, G=k, model="St", criterion="BIC")
  if (verbose) {
    report <- list("locations" = cl$mu, "Sigmas" = cl$Sigma, "skew" = cl$beta, "df" = cl$df)
    print(report)
  }
  cl
}


#' Mixed discrete and continuous latent trait
#' 
#' @examples
#' data(veronica)
#' v2 <- data.frame(lapply(veronica, as.factor))
#' set.seed(887766)
#' cl <- cluster_discretemixture(v2, verbose=TRUE, bic_plot=TRUE)
#' @export
cluster_discretemixture <- function(data, k=1:10, verbose = FALSE, bic_plot=FALSE){
  # reorder the dataset
  f <- sapply(data, is.factor)
  factors <- data[f]
  numerics <- data[!f]
  m <- as.matrix(cbind(numerics, factors))
  cl <- fpc::flexmixedruns(m,
                           continuous = ncol(numerics),
                           discrete = ncol(factors),
                           n.cluster = k,
                           verbose = verbose)
  optimal_k <- k[which.min(cl$bicvals)]
  if (bic_plot){
    plot(k, cl$bicvals, type="l", ylab="BIC", xlab="Number of clusters")
    abline(v=optimal_k, col="red", lty="dotted")
  }
  list("fm" = cl$flexout[[optimal_k]],
       "cluster" = cl$flexout[[optimal_k]]@cluster)
}

