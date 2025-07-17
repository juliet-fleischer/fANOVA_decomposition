# install.packages("remotes")
# remotes::install_github("zmjones/mmpf/pkg")
# remotes::install_github("zmjones/fanova")
library(fanova)
library(randomForest)
library(ggplot2)
library(data.table)
library(MASS)
library(mvtnorm)

theme_set(
  theme_minimal(base_size = 11) +
    theme(
      plot.title = element_blank(),         # No title (handled by LaTeX caption)
      axis.title = element_text(size = 11), # Axis labels
      axis.text = element_text(size = 10),  # Tick labels
      panel.grid.major = element_line(color = "grey85", size = 0.3),
      panel.grid.minor = element_blank(),
      axis.line = element_line(color = "black", linewidth = 0.3),
      panel.border = element_blank(),
      legend.position = "none"              # No legend (unless explicitly needed)
    )
)


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
weight_fun_ind <- function(design, data) {
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

m_indp = lm(y ~ x1 * x2, data = df)
fa1 = functionalANOVA(g.features, c("x1", "x2"), c(100, 2), m_indp, weight.fun = weight_fun_ind)
print(fa1)

plt1 = melt(fa1[fa1$effect %in% c("x1", "x2"), ],
            id.vars = c("f", "effect"), na.rm = TRUE)
indep_150_main = ggplot(plt1, aes(value, f)) +
   geom_line() + facet_wrap(~ variable, scales = "free_x") +
  geom_abline(data = data.frame(variable = c("x1", "x2"), slope = c(1, 2)),
              aes(slope = slope, intercept = 0, color = variable),
              show.legend = FALSE)
indep_150_interact = ggplot(fa1[fa1$effect == "x1:x2", ], aes(x1, x2, z = f, fill = f)) +
  geom_raster() +
  stat_contour(aes(z = f), color = "white", alpha = 0.5)


## Dependent Inputs ----

# set seed and create data vectors
sample_covariance_matrix1 <- matrix(c(1, 0.5, 0.5, 1), # update sample cov matrix for dependence
                                   ncol = 2)

# create bivariate normal distribution
sample_distribution1 <- mvrnorm(n = sample_size,
                               mu = sample_meanvector, 
                               Sigma = sample_covariance_matrix1)

# joint density which will serve as weight function
weight_fun_dep <- function(design, data) {
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

m_dep = lm(y ~ x1 * x2, data = df1)
fa2 = functionalANOVA(g.features1, c("x1", "x2"), c(100, 2), m_dep, weight.fun = weight_fun_dep)
print(fa2)

plt2 = melt(fa2[fa2$effect %in% c("x1", "x2"), ],
            id.vars = c("f", "effect"), na.rm = TRUE)
dep_150_main = ggplot(plt2, aes(value, f)) + geom_line() + facet_wrap(~ variable, scales = "free_x")
dep_150_interact = ggplot(fa2[fa2$effect == "x1:x2", ], aes(x1, x2, z = f, fill = f)) +
  geom_raster() +
  stat_contour(aes(z = f), color = "white", alpha = 0.5)



