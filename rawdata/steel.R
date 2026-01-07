steel <- read.csv(file = "steelplatesfaults.csv", header = TRUE)
save("steel", file = "../data/steel.rda", compress = "bzip2")
