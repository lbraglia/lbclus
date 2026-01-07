#' @export
dist_eucl <- function(x) stats::dist(x = x, method = "euclidean")

#' @export
dist_minkowski <- function(x, p = 2) stats::dist(x = x, method = "minkowski", p = p)


#' Squared mahalanobis distance
#' 
#' @examples
#' 
#' olive <- oliveoil[, 3:10]
#' dm <- dist_mahalanobis(olive)
#' sel <- 1:5
#' as.matrix(dm)[sel, sel]
#' 
#' @export
dist_mahalanobis <- function(x) {
  covx <- cov(x)
  md <- apply(x, 1, function(obs) stats::mahalanobis(x, obs, covx))
  as.dist(md)
}


#' @export
dist_manhattan <- function(x) stats::dist(x = x, method = "manhattan")


#' Simple matching distance
#' @examples
#' a <- dist_smd(veronica)
#' b <- dist(veronica, method = "manhattan")/ncol(veronica) # just in binary case it's the same
#' sel <- 1:5
#' as.matrix(a)[sel, sel]
#' as.matrix(b)[sel, sel]
#' @export
dist_smd <- function(x) nomclust::sm(x)

#' @export
dist_jaccard <- function(x) stats::dist(x = x, method = "binary")

#' Correlation distance 2
#'
#' @examples
#' b <- bundestag[1:5]
#' dist_corr1(b)
#' 
#' @export
dist_corr1 <- function(x) {
  res <- 0.5 * (1 - cor(x))
  as.dist(res)
}

#' Correlation distance 2
#' 
#' @examples
#' b <- bundestag[1:5]
#' dist_corr2(b)
#' 
#' @export
dist_corr2 <- function(x) {
  res <- 1 - abs(cor(x))
  as.dist(res)
}


#' @export
dist_gower <- function(x, type = list(ordinal = seq_len(ncol(x)))) cluster::daisy(x, metric = "gower", type = type)
