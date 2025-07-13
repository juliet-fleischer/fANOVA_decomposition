# remotes::install_github("guillermozbta/mir")
library(mlr)

# Simulated example using function g(x1, x2) with normally distributed inputs
set.seed(1)
g <- function(x1, x2) {
  x1 + 2 * x2 + x1 * x2
}

n <- 200
x1 <- rnorm(n)
x2 <- rnorm(n)
y <- g(x1, x2)

dat <- data.frame(x1 = x1, x2 = x2, y = y)
task <- makeRegrTask(data = dat, target = "y")
lrn <- makeLearner("regr.rpart")
fit <- train(lrn, task)

fa <- generateFunctionalANOVAData(fit, task, c("x1", "x2"), depth = 2L)
plotPartialDependence(fa)



