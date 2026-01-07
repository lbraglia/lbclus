stars <- read.table(file = "stars5000.dat", header = TRUE)
save("stars", file = "../data/stars.rda", compress = "bzip2")
