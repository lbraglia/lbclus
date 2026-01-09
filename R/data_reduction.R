#' PCA 2-d coordinates
#' 
#'@export
coords_pca <- function(x, scale = TRUE, verbose = TRUE){
  if (scale) x <- scale(x)
  pca <- stats::princomp(x)
  if (verbose) print(summary(pca)) # print pca stats
  pca$scores[, 1:2]
}

#' Quantifying information loss (stress) using PCA derived variables
#'
#' Useful for obtaining data (choosing a small p) for following clustering not
#' having a too much high info loss
#' 
#' @examples
#' data(oliveoil)
#' olive <- oliveoil[, 3:10]
#' infoloss_pca(olive, scale = FALSE)
#' infoloss_pca(olive, scale = TRUE)
#' @export
infoloss_pca <- function(x, scale = TRUE, verbose = FALSE){
  if (scale) x <- scale(x)
  pca <- stats::princomp(x)
  if (verbose) print(summary(pca))
  vars <- pca$sdev^2
  1 - (cumsum(vars) / sum(vars))
}


#' MDS 2-d coordinates
#' 
#' @examples
#' data(bundestag)
#' # bundestag example
#' b <- bundestag[1:5]
#' d <- dist_corr1(b)
#' coords_mds(d)
#'
#' # veronica example
#' data(veronica)
#' d <- dist_jaccard(veronica)
#' plot(coords_mds(d), asp=1)
#' @export
coords_mds <- function(d, verbose = TRUE){
  res <- smacof::mds(as.matrix(d), ndim = 2)
  if (verbose) cat(sprintf("Stress is %.4f\n\n", res$stress))
  res$conf
}




#' Quantifying information loss (stress) using MDS derived variables
#'
#' Useful for obtaining data (choosing a small p) for following clustering not having a
#' too much high info loss
#' 
#' @examples
#' data(veronica)
#' d <- dist_jaccard(veronica)
#' infoloss_mds(d)
#' @export
infoloss_mds <- function(d, ndim = 1:10){
  names(ndim) <- ndim
  sapply(ndim, function(n) smacof::mds(as.matrix(d), ndim = n)$stress)
}
