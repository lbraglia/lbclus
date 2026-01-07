clusterdata1 <- read.table(file = "clusterdata1.dat", as.is = TRUE, header = TRUE)
# names(clusterdata1) <- tolower(names(clusterdata1))
save("clusterdata1", file = "../data/clusterdata1.rda", compress = "bzip2")
