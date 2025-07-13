# remotes::install_github("guillermozbta/mir")
library(mlr)

fit = train("regr.rpart", bh.task)
fa = generateFunctionalANOVAData(fit, bh.task, c("lstat", "rm"), depth = 2L)
plotPartialDependence(fa)


