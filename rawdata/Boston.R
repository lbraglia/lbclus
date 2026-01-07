housing <- read.table(file = "Boston.dat", header = TRUE)
save("housing", file = "../data/housing.rda", compress = "bzip2")
