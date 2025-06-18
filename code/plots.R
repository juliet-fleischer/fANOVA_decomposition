# Load necessary libraries
library(ggplot2)
library(reshape2)
library(plotly)
library(tidyverse)
library(gridExtra)

# Inputs ----
## Main effect inputs
x_seq <- seq(0, 2, length.out = 100)
# Main effects: 1D plots with unique values only
df_f1 <- data.frame(x1 = x_seq, f1 = x_seq)
df_f2 <- data.frame(x2 = x_seq, f2 = 2 * x_seq)

## Interaction effect inputs
# Create a full grid over [0, 2]^2
x_seq <- seq(0, 2, length.out = 100)
grid <- expand.grid(x1 = x_seq, x2 = x_seq)
grid$f12 <- grid$x1 * grid$x2  # interaction term

# Convert to matrix for surface plotting
z_matrix <- matrix(grid$f12, nrow = 100, ncol = 100)

## 1) Independent variables
# Plot main effects properly
p1 <- ggplot(df_f1, aes(x = x1, y = f1)) +
  geom_line(color = "blue") + theme_minimal() +
  labs(title = "Main Effect f1(x1)", x = "x1", y = "f1(x1)") +
  ylim(0, 2)

p2 <- ggplot(df_f2, aes(x = x2, y = f2)) +
  geom_line(color = "red") + theme_minimal() +
  labs(title = "Main Effect f2(x2)", x = "x2", y = "f2(x2)") +
  ylim(0, 2)

# plot p1 and p2 side by side
grid.arrange(p1, p2, ncol = 1)
df <- data.frame(x1 = x_seq, x2 = x_seq,
f1 = x_seq, f2 = 2 * x_seq, f12 = x_seq * x_seq)

# Plot 3D surface plot for the interaction effect
plot_ly(
  x = x_seq, y = x_seq, z = ~z_matrix,
  type = "surface"
) %>%
  layout(
    title = "Interaction Effect f12(x1,x2) = x1 * x2",
    scene = list(
      xaxis = list(title = "x1"),
      yaxis = list(title = "x2"),
      zaxis = list(title = "f12(x1, x2)")
    )
  )

  ## 2) Dependent variables