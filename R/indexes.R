#' Adjusted Rand Index (agreement of clustering solutions like CCC)
#'
#' @examples
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
#' index_ari(olive3$cluster, oliveoid$macro.area)
#' @export
index_ari <- function(cl_x, cl_y) mclust::adjustedRandIndex(cl_x, cl_y)



#' Mean purity index
#'
#' Used in homework 1 to compare clustering solutions to a known classification
#' in a supervised approach: the index close to 1 suggest that clustering is
#' reproducing the reference/known classification better
#' @examples
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

