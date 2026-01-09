## Colors and symbols

cluster_pch <- fpc::clusym  # choose pch
set.seed(1)
cluster_col <- sample(grDevices::colors(), length(cluster_pch)) # choose colors

#' Function for plotting clusters
#' @examples
#' example(cluster_kmeans)
#' @export
plot_cluster <- function(coords, clvec, col_alpha = 1, ...){
  clustering_col <- lbmisc::col2hex(cluster_col[clvec], alpha = col_alpha)
  clustering_pch <- cluster_pch[clvec]
  plot(coords, col = clustering_col, pch = clustering_pch, ...)
}


#' compute cluster profiles (means)
#' @examples
#' # example1
#' example(cluster_kmeans)
#' # example2 (bad but quick example)
#' airquality2 <- airquality
#' airquality2$Month <- factor(airquality2$Month)
#' airquality2$Day <- factor(airquality2$Day)
#' cluster_profiles(airquality2, airquality2$Month) # suppose month is result of a clustering
#'@export
cluster_profiles <- function(data, clvec){
  factors <- sapply(data, is.factor)

  if (any(factors)){
    # handle factors
    data_factors <- data[, factors]
    col_perc <- function(x, xn) {
      rval <- prop.table(table(x, clvec), margin = 2) * 100
      rownames(rval) <- sprintf("%s %s", xn, rownames(rval))
      rval
    }
    percs <- Map(col_perc, data_factors, as.list(names(data_factors)))
    percs <- do.call(rbind, percs)
  }

  # numerics
  data_notf <- data[, !factors]
  n <- aggregate(data.frame("n" = rep(1, nrow(data))), by = list(clvec), FUN = sum)
  res <- aggregate(data_notf, by = list(clvec), FUN = mean, na.rm = TRUE)
  means <- merge(n, res, by = "Group.1", all.x = TRUE)
  names(means)[1] <- "cluster"
  # return results
  if (any(factors)) list("means" = means, "factor_percs" = percs) else means
}



### battery-included pairsplot
### ------------------------------------------------------
pairs_diagonal <- function(x, ...){
    # same as from ?pairs
    usr <- par("usr");  on.exit(par(usr))
    par(usr = c(usr[1:2], 0, 1.5))
    h <- hist(x, plot = FALSE)
    breaks <- h$breaks
    nB <- length(breaks)
    y <- h$counts
    y <- y/max(y)
    rect(breaks[-nB], 0, breaks[-1], y, col="grey")
}

## in doing pairs, in the upper panel, show bivariate correlation
## (more visible as nearer to 1 in abs value)
panel_cor <- function(x, y, digits = 2, prefix = "", ...)
{
    ## slightly different from ?pairs
    par(usr = c(0, 1, 0, 1))
    r <- cor(x, y, method = "pearson", use="complete.obs")
    txt <- format(c(r, 0.123456789), digits = digits)[1]
    txt <- paste0(prefix, txt)
    col <- lbmisc::col2hex("black", abs(r))
    text(0.5, 0.5, txt, col = col)
}


#' pairs with smoothing, histograms and less/more visible correlations
#' @param data dataframe
#' @param pch point type
#' @param col color
#' @param ... other parameters given to pairs
#' @examples
#' pairsplot(airquality, col=airquality[, 5])
#' @export
plot_pairs <-function(data, pch=20, col=lbmisc::col2hex("black", 0.3), ...){
  graphics::pairs(data,
                  pch=pch,
                  col=col,                 
                  panel = panel.smooth, #add smothing bivariate relation
                  diag.panel = pairs_diagonal, #histogram
                  upper.panel = panel_cor, # correlation
                  ...)
}
