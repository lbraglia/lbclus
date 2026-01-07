#' MDS 2-d coordinates
#' 
#' @examples
#'
#' # bundestag example
#' b <- bundestag[1:5]
#' d <- dist_corr1(b)
#' coords_mds(d)
#'
#' # veronica example
#' d <- dist_jaccard(veronica)
#' plot(coords_mds(d), asp=1)
#' @export
coords_mds <- function(d, verbose = TRUE){
  res <- smacof::mds(as.matrix(d), ndim = 2)
  if (verbose) cat(sprintf("Stress is %.4f\n\n", res$stress))
  res$conf
}

#' PCA 2-d coordinates
#' 
#'@export
coords_pca <- function(x, scale = TRUE, verbose=TRUE){
  if (scale) x <- scale(x)
  pca <- stats::princomp(x)
  if (verbose) print(summary(pca)) # print pca stats
  pca$scores[, 1:2]
}

#'@export
scale_mad <- function(x, center = TRUE, scale = TRUE){
  if (center) x <- x - median(x, na.rm = TRUE)
  if (scale)  x <- x / mad(x, na.rm = TRUE)
  x
}
