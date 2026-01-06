
set.seed(14526)
# devtools::install_github("lbraglia/lbclus")
# https://github.com/lbraglia/lbclus
library(lbclus)

db <- read.table(, header=TRUE)

head(db)
summary(db)
sapply(db, sd)
cor(db)

lbclus::pairplot(db)
