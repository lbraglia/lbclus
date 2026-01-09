#' @export
dist_eucl <- function(x) stats::dist(x = x, method = "euclidean")

#' @export
dist_minkowski <- function(x, p = 2) stats::dist(x = x, method = "minkowski", p = p)


#' Squared mahalanobis distance
#' 
#' @examples
#' data(oliveoil)
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
#' data(veronica)
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
#' data(bundestag)
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
#' data(bundestag)
#' b <- bundestag[1:5]
#' dist_corr2(b)
#' 
#' @export
dist_corr2 <- function(x) {
  res <- 1 - abs(cor(x))
  as.dist(res)
}

#' Gower distance
#'
#' 
#' @examples
#' 
#' data(housing)
#' head(housing) # chas (4-th) is binary, rad (9-th) is ordinal
#' summary(housing)
#' 
#' # binary: simple matching distance (symm)
#' # binary: jaccard (asymm)
#' # nominal: simple matching distance (symm). Factor are handled like that
#' # ordered: coded with integer and progressive and treated as numerical
#' # unspecified: numerical with L_1
#'
#' # utility function
#' sneakpeek <- function(x) { sel <- 1:5 ; as.matrix(x)[sel, sel] }
#'
#' # all numerical (L1)
#' d0 <- dist_gower(housing)
#' sneakpeek(d0) # it complains
#' 
#' # chas with simple matching distance
#' typelist1 <- list(symm=4)
#' d1 <- dist_gower(housing, typelist1)
#' sneakpeek(d1)
#'
#' # chas and rad with simple matching distance (4th) rad (9th)
#' typelist2 <- list(symm=c(4,9))
#' d2 <- dist_gower(housing, typelist2) # this doesnt' work
#' housing$rad <- as.factor(housing$rad) # factor should be simple matching distanced 
#' d2b <- dist_gower(housing, typelist1)
#' sneakpeek(d2b)
#' d2c <- dist_gower(housing, typelist2) # doesn't work...
#' 
#' # chas (4th) with simple matching distance and ordering rad (9th)
#' housing$rad <- as.ordered(housing$rad)
#' d3 <- dist_gower(housing, typelist1)
#' sneakpeek(d3) # no changes with d1 bc ordered are basically L1 treated
#' 
#' # Take home messages: just ..
#' # - change to factor or ordered as appropriate
#' # - for binary variable choose jaccard or simple matching distance
#' @export
dist_gower <- function(x, type = list()) cluster::daisy(x, metric = "gower", type = type)
