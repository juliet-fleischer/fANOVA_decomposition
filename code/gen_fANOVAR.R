library(MASS)
library(mvtnorm)
library(ggplot2)

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


# Approximate method ----
## Setup ----
n <- 30
x1 <- seq(-3, 3, length.out = n)
x2 <- seq(-3, 3, length.out = n)
grid <- expand.grid(x1 = x1, x2 = x2)

a <- 3
g <- function(x1, x2) {
  a + x1 + 2 * x2 + x1 * x2
}
grid$gval <- with(grid, g(x1, x2))

rho <- 0.6
Sigma <- matrix(c(1, rho, rho, 1), 2, 2)
grid$density <- mapply(function(x1, x2) {
  dmvnorm(c(x1, x2), mean = c(0, 0), sigma = Sigma)
}, grid$x1, grid$x2)

# X: design matrix (dummy-coded main + interaction)
# F: observed function values
# W: diagonal matrix of densities

grid$x1 <- factor(grid$x1)
grid$x2 <- factor(grid$x2)

lm_fit <- lm(
  gval ~ x1 * x2,
  data = grid,
  weights = grid$density,
  contrasts = list(
    x1 = contr.sum(length(unique(grid$x1))),
    x2 = contr.sum(length(unique(grid$x2)))
  )
)
grid$fitted <- fitted(lm_fit)

## Compute the fANOVA terms ----
y0 <- coef(lm_fit)[["(Intercept)"]]

main1 <- aggregate(fitted ~ x1, data = grid, FUN = mean) # marginalizes the function
main1$y1 <- main1$fitted - y0
main2 <- aggregate(fitted ~ x2, data = grid, FUN = mean)
main2$y2 <- main2$fitted - y0
# Merge back main effects
grid <- merge(grid, main1[, c("x1", "y1")], by = "x1")
grid <- merge(grid, main2[, c("x2", "y2")], by = "x2")

grid$y12 <- grid$fitted - y0 - grid$y1 - grid$y2


## Visualization of the fANOVA terms ----
main1$x1 <- as.numeric(as.character(main1$x1))
ggplot(main1, aes(x = x1, y = y1)) +
  geom_point() +
  labs(title = "Main Effect y1(x1)", x = "x1", y = "y1") +
  ylim(-6, 6)

main2$x2 <- as.numeric(as.character(main2$x2))
ggplot(main2, aes(x = x2, y = y2)) +
  geom_point() +
  labs(title = "Main Effect y2(x2)", x = "x2", y = "y2")

grid$x1 <- as.numeric(as.character(grid$x1))
grid$x2 <- as.numeric(as.character(grid$x2))
ggplot(grid, aes(x = x1, y = x2, fill = y12)) +
  geom_tile() +
  labs(title = "Interaction Effect y12(x1, x2)", x = "x1", y = "x2", fill = "y12") +
  scale_fill_viridis_c()

# contour plot for interaction effect
ggplot(grid, aes(x = x1, y = x2, z = y12)) +
  geom_contour_filled() +
  labs(title = "Contour Plot of Interaction Effect", x = "x1", y = "x2") +
  scale_fill_viridis_d() +
  theme(legend.position = "right")



