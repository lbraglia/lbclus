## Colors and symbols

#'@export
cluster_pch <- fpc::clusym  # choose pch

set.seed(1)

#'@export
cluster_col <- sample(grDevices::colors(), length(cluster_pch)) # choose colors

## descrittive

#' compute cluster profiles (means)
#' @examples
#' cluster_profiles(airquality[, -(5:6)], airquality$Month)
#'@export
cluster_profiles <- function(data, clvec){
  res <- aggregate(data, by = list(clvec), FUN = mean, na.rm = TRUE)
  n <- aggregate(data.frame("n" = rep(1, nrow(data))), by = list(clvec), FUN = sum)
  rval <- merge(n, res, by = "Group.1", all.x = TRUE)  
  names(rval)[1] <- "cluster"
  rval
}



#' K-means with better defaults
#' 
#'@export
kmeans <- function(x, k, nstart = 100, iter.max = 100)
  stats::kmeans(x, centers = k, nstart = nstart, iter.max = iter.max)



#' Function for plotting clusters
#' 
#'@export
plot_clus <- function(coords, clvec, col_alpha = 1, ...){
  clustering_col <- lbmisc::col2hex(cluster_col[clvec], alpha = col_alpha)
  clustering_pch <- cluster_pch[clvec]
  plot(coords, col = clustering_col, pch = clustering_pch, ...)
}


#' Adjusted Rand Index
#' 
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





## ------------------------ gapnc number of cluster with gap statistics


## #' Function for plotting elbow plot for k-means clustering
## #' 
## #'@examples
## #' clusterdata1 <- read.table("data/clusterdata1.dat")
## #' plot_km_elbow(clusterdata1)
## #'
## #'@export
## plot_km_elbow <- function(data, k = 1:10, ...){
##   K <- setNames(as.list(k), k)
##   res <- lapply(K, function(x) km(data, x)$tot.withinss)
##   plot(k, unlist(res), xlab = "k", ylab = "S_k", type = "l", ...)
## }


#' gapnc function with minimal changes for personal use/automation
#'
#' use by default kmeans with better defaults (eg iter.max
#'
#' @examples
#' # clusterdata2 <- read.table("data/clusterdata2.dat")
#' set.seed(112)
#' gap_clus2 <- gapnc(clusterdata2, K.max = 15)
#' plot(gap_clus2)
#' # optimal number of cluster is 9 o 10 a seconda di come si sveglia
#' 
#' @export
gapnc <- function(data,
                  FUNcluster = lbclus::kmeans, # use kmeans with better defaults
                  K.max = 10,             # max number
                  B = 100,                # 
                  d.power = 2,            #
                  spaceH0 = "scaledPCA",  # 
                  method = "globalSEmax", # maxSE params
                  SE.factor = 2,          # 
                  ...) # options passed to FUNcluster
{
    spaceH0 <- match.arg(spaceH0, c("scaledPCA", "original"))
    method <- match.arg(method, c("firstSEmax", "Tibs2001SEmax", "globalSEmax", "firstmax", "globalmax"))

    ## gap statistics
    gap <- cluster::clusGap(x = data,
                            FUNcluster = FUNcluster,
                            K.max = K.max,
                            B = B,
                            d.power = d.power,
                            spaceH0 = spaceH0,
                            ...)

    ## Find optimal number of clusters
    nc <- cluster::maxSE(gap$Tab[, 3], gap$Tab[, 4],
                         method = method,
                         SE.factor = SE.factor)

    ## Re-run clustering (kmeans) with optimal number of cluster.
    optimal_cl <- FUNcluster(data, nc, ...)

    ## return
    res <- list(
        ## results
        "gap" = gap,
        "n_cl" = nc,
        "optimal_cl" = optimal_cl,
        ## inputs
        "data" = data,
        "FUNcluster" = FUNcluster,
        "FUNcluster_params" = list(...),
        "K.max" = K.max,
        "B" = B,
        "d.power" = d.power,
        "spaceH0" = spaceH0,
        "method" = method,
        "SE.factor" = SE.factor)
    class(res) <- c("gapAnalysis", "list")
    res
}

#' @export
plot.gapAnalysis <- function(x) {
    ## id for clusters (x on plotting)
    clus <- seq(1, x$K.max)

    ## Actual plotting 
    par(mfrow = c(1, 3))

    ## 1) Values of gap and +/- 1se bands and optimal K
    plot(x$gap, main = "Gap +/- 1*se bands")
    abline(v = x$n_cl, lty = "dotted", col = "blue")

    ## 2) elbow method: values of S_k and optimal K (according to gap analysis)
    plot(clus, exp(x$gap$Tab[, 1]),
         xlab = "k", ylab = "S_k", type = "l",
         main = "Elbow method")
    abline(v = x$n_cl, lty = "dotted", col = "blue")

    ## 3) log S_k and optimal k
    ## determining the ylim first
    tmp <- x$gap$Tab[, 1:2]
    dim(tmp) <- NULL
    logsk_plot_ylim <- range(tmp)
    plot(clus, x$gap$Tab[, 1],  type = "l", # 3a)  in the data
         xlab = "k", ylab = "log(S_k)", main = "log S_k plot",
         ylim = logsk_plot_ylim)
    points(clus, x$gap$Tab[, 2], type = "l", lty = 2) # 3b) under unif
    abline(v = x$n_cl, lty = "dotted", col = "blue")
    legend("topright",
           legend = c("log(S_k) in data", "E(log(S_k)) uniform"),
           lty = 1:2, bg = "white")
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
pairsplot <-function(data, pch=20, col=lbmisc::col2hex("black", 0.3), ...){
  graphics::pairs(data,
                  pch=pch,
                  col=col,                 
                  panel = panel.smooth, #add smothing bivariate relation
                  diag.panel = pairs_diagonal, #histogram
                  upper.panel = panel_cor, # correlation
                  ...)
}
