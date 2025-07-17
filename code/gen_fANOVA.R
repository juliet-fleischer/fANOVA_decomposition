# install.packages("remotes")
# remotes::install_github("zmjones/mmpf/pkg")
# remotes::install_github("zmjones/fanova")
library(fanova)
library(randomForest)
library(ggplot2)
library(data.table)
library(MASS)
library(mvtnorm)

# Functions ----
a = 0
g <- function(x1, x2) {
  a + x1 + 2 * x2 + x1 * x2
}


# Example 1 ----
## Independent inputs ----
# simulate standard normal inputs, i.e. x1, x2 ~ N(0, 1)
set.seed(9898)
sample_size <- 100                                       
sample_meanvector <- c(0, 0)                                   
sample_covariance_matrix <- matrix(c(1, 0, 0, 1), # independent
                                   ncol = 2)

# create bivariate normal distribution
sample_distribution <- mvrnorm(n = sample_size,
                               mu = sample_meanvector, 
                               Sigma = sample_covariance_matrix)

# joint density which will serve as weight function
weight_fun <- function(design, data) {
  mvtnorm::dmvnorm(
    x     = as.matrix(design[, c("x1", "x2")]),
    mean  = sample_meanvector,
    sigma = sample_covariance_matrix
  )
}

df <- data.frame(
  x1 = sample_distribution[, 1],
  x2 = sample_distribution[, 2],
  y = g(sample_distribution[, 1], sample_distribution[, 2])
)
g.features = df[, c("x1", "x2")]
g.target = df$y

m = randomForest(g.features, g.target)

fa = functionalANOVA(g.features, c("x1", "x2"), c(100, 2), m, weight.fun = weight_fun)
print(fa)

plt = melt(fa[fa$effect %in% c("x1", "x2"), ],
           id.vars = c("f", "effect"), na.rm = TRUE)
gg1_1 = ggplot(plt, aes(value, f)) +
  geom_point() + 
  geom_line() + 
  facet_wrap(~ variable, scales = "free_x") +
  geom_abline(data = data.frame(variable = c("x1", "x2"), slope = c(1, 2)),
              aes(slope = slope, intercept = 0, color = variable),
              show.legend = FALSE)

gg1_2 = ggplot(fa[fa$effect == "x1:x2", ], aes(x1, x2, z = f, fill = f)) + geom_raster()

# not really centred around zero?
# also the interaction effects are kind of "shifted"?


### Question ----
# instead of random forest fit a linear model with interaction
m1 = lm(y ~ x1 * x2, data = df)
fa1 = functionalANOVA(g.features, c("x1", "x2"), c(150, 2), m1, weight.fun = weight_fun)
print(fa1)

plt1 = melt(fa1[fa1$effect %in% c("x1", "x2"), ],
            id.vars = c("f", "effect"), na.rm = TRUE)
ggplot(plt1, aes(value, f)) +
  geom_point() + geom_line() + facet_wrap(~ variable, scales = "free_x")
ggplot(fa1[fa1$effect == "x1:x2", ], aes(x1, x2, z = f, fill = f)) + geom_raster()


## Dependent Inputs ----

# set seed and create data vectors
sample_covariance_matrix1 <- matrix(c(1, 0.5, 0.5, 1), # update sample cov matrix for dependence
                                   ncol = 2)

# create bivariate normal distribution
sample_distribution1 <- mvrnorm(n = sample_size,
                               mu = sample_meanvector, 
                               Sigma = sample_covariance_matrix1)

# joint density which will serve as weight function
weight_fun <- function(design, data) {
  mvtnorm::dmvnorm(
    x     = as.matrix(design[, c("x1", "x2")]),
    mean  = sample_meanvector,
    sigma = sample_covariance_matrix1
  )
}

df1 <- data.frame(
  x1 = sample_distribution1[, 1],
  x2 = sample_distribution1[, 2],
  y = g(sample_distribution1[, 1], sample_distribution1[, 2])
)
g.features1 = df1[, c("x1", "x2")]
g.target1 = df1$y

m2 = lm(y ~ x1 * x2, data = df1)
fa2 = functionalANOVA(g.features1, c("x1", "x2"), c(100, 2), m2, weight.fun = weight_fun)
print(fa2)

plt2 = melt(fa2[fa2$effect %in% c("x1", "x2"), ],
            id.vars = c("f", "effect"), na.rm = TRUE)
gg2_1 = ggplot(plt2, aes(value, f)) +
  geom_point() + geom_line() + facet_wrap(~ variable, scales = "free_x")
gg2_2 = ggplot(fa2[fa2$effect == "x1:x2", ], aes(x1, x2, z = f, fill = f)) + geom_raster()



