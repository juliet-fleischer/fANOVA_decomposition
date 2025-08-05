# Load necessary libraries
library(ggplot2)
library(plotly)
library(tidyverse)


theme_pub <- theme_minimal(base_size = 12) +
  theme(
    plot.title = element_blank(),
    legend.title = element_text(size = 11),
    legend.text = element_text(size = 10),
    axis.title = element_text(size = 11),
    axis.text = element_text(size = 10),
    panel.grid.major = element_line(color = "grey90", size = 0.3),
    panel.grid.minor = element_blank()
  )



# Standard MVN input (with varying rho) ----
## Setup
x_seq <- seq(-3, 3, length.out = 100)
rho = 0.5 # only if rho = 0 do we get fANOVA components

df_wide <- tibble(
  x = x_seq,
  y1 = x_seq + rho * (2 * x_seq + x_seq^2 - 1),
  y2 = 2 * x_seq + rho * (x_seq + x_seq^2 - 1)
)

df_long <- df_wide %>%
  pivot_longer(
    cols = c(y1, y2),
    names_to = "Effects",
    values_to = "y"
  )

main_colors <- c(
  "y1" = "#FDD9A0",  # light peach
  "y2" = "#C2A5CF"   # dusty lavender
)

dep_hoeffding_main <- ggplot(df_long, aes(x = x, y = y, color = Effects)) +
  geom_line(size = 2) +
  labs(x = expression(X[i]), y = "Effect", color = "term") +
  scale_color_manual(
    values = main_colors,
    labels = c(
      expression(tilde(h)[{"{1}"}]),
      expression(tilde(h)[{"{2}"}])
    )
  ) +
  theme_pub


# Filled contour plot ----

x_seq <- seq(-1.5, 1.5, length.out = 200)
grid <- expand.grid(x1 = x_seq, x2 = x_seq)
grid$f12 <- with(grid, x1 * x2 - 2 * rho * x1 - rho * x2 - rho * x1^2 - rho * x2^2 + rho)


dep_hoeffding_interaction <- ggplot(grid, aes(x = x1, y = x2, z = f12)) +
  geom_contour_filled() +
  scale_fill_brewer(palette = "YlGn") +
  labs(x = expression(X[1]), y = expression(X[2]), fill = "Interaction") +
  theme_pub


# Grid for interaction effect
grid_seq <- seq(-3, 3, length.out = 100)

z_matrix <- outer(
  grid_seq, grid_seq,
  FUN = function(x1, x2) {
    x1 * x2 - 2 * rho * x1 - rho * x2 - rho * x1^2 - rho * x2^2 + rho
  }
)


# 3D surface plot ----
p_interaction <- plot_ly(
  x = grid_seq, y = grid_seq, z = ~z_matrix,
  type = "surface", colorscale = "RdBu"
) %>%
  layout(
    scene = list(
      xaxis = list(title = "x1"),
      yaxis = list(title = "x2"),
      zaxis = list(title = "y12(x1, x2)")
    )
  )

# Show the 3D plot
p_interaction


