## ---------- supervised

#' Adjusted Rand Index (agreement of clustering solutions like CCC)
#'
#' @examples
#' 
#' data(oliveoil)
#' olive <- oliveoil[-c(1:2)]
#' solive <- scale(olive)
#' olive3 <- lbclus::kmeans(olive, 3)
#' olive3s <- lbclus::kmeans(solive, 3)
#'
#' # agreement between clustering solutions
#' index_ari(olive3$cluster, olive3s$cluster)
#' 
#' # comparison with known classification (higher better)
#' index_ari(olive3$cluster, oliveoil$macro.area)
#' index_ari(olive3s$cluster, oliveoil$macro.area)
#' 
#' @export
index_ari <- function(cl_x, cl_y) mclust::adjustedRandIndex(cl_x, cl_y)



#' Mean purity index
#'
#' Used in homework 1 to compare clustering solutions to a known classification
#' in a supervised approach: the index close to 1 suggest that clustering is
#' reproducing the reference/known classification better
#' @examples
#'
#' data(oliveoil)
#' # oliveoil <- read.table("data/oliveoil.dat", header=TRUE)
#' head(oliveoil)
#' olive <- oliveoil[-c(1:2)]
#' cl <- kmeans(olive, 9)$cluster
#' index_mean_purity(oliveoil$region, cl)
#' 
#' @export
index_mean_purity <- function(ref, cl){
  tab <- table(ref, cl)
  purity <- function(x) sum( (x/sum(x))^2  )
  purity_of_each_cluster <- apply(tab, 2, purity)
  weighted.mean(purity_of_each_cluster, w = colSums(tab))
}


## ---------- UNsupervised

#' Average Silhouette Width
#' 
#' Quality of clustering solutions given distance between units (the higher the
#' better)
#' 
#' @param cl a classification vector (integers) or a list with $clustering component
#' @param d dist object
#' @examples
#'
#' data(bundestag)
#' b <- bundestag[1:5]
#' d <- dist_manhattan(b)
#' bpam <- lbclus::pam(d, k=2)
#' 
#' index_asw(bpam$cluster, d=d) # should be 0.48
#' index_asw(bpam, d=d) #same?
#'
#' # choosing n of clusters within k-means
#' # -------------------------------------
#' data(clusterdata1)
#' plot(clusterdata1)
#' sdat <- scale(clusterdata1)
#' sdat_dist <- dist(sdat)
#' 
#' K <- 2:5
#' names(K) <- K
#' clusters <- lapply(K, function(k) lbclus::kmeans(sdat, k=k)$cluster)
#' sapply(clusters, function(cl) index_asw(cl = cl, d = sdat_dist))
#' 
#' @export
index_asw <- function(cl, d) summary(cluster::silhouette(x = cl, dist = d))$avg.width

