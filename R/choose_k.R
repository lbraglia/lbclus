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
#' data(clusterdata2)
#' set.seed(112)
#' gap_clus2 <- gapnc(clusterdata2, K.max = 15) # optimal: 9 or 10
#' @export
gapnc <- function(data,
                  FUNcluster = lbclus::cluster_kmeans, # use kmeans with better defaults
                  K.max = 10,             # max number
                  B = 100,                # 
                  d.power = 2,            #
                  spaceH0 = "scaledPCA",  # 
                  method = "globalSEmax", # maxSE params
                  SE.factor = 2,          #
                  view = TRUE,            # do the plotting
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

    ## Re-run clustering (kmeans) with optimal number of cluster. naa
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
    if (view) plot(res)
    res
}

#' @export
plot.gapAnalysis <- function(x) {
    ## id for clusters (x on plotting)
    clus <- seq(1, x$K.max)

    ## Actual plotting 
    par(mfrow = c(1, 3))

    ## 1) elbow method: values of S_k and optimal K (according to gap analysis)
    plot(clus, exp(x$gap$Tab[, 1]),
         xlab = "k", ylab = "S_k", type = "l",
         main = "Elbow method")
    ## abline(v = x$n_cl, lty = "dotted", col = "blue")

    ## 2) Values of gap and +/- 1se bands and optimal K
    plot(x$gap, main = "Gap +/- 1*se bands")
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
