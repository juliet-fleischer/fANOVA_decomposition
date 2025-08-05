# Load necessary libraries
library(ggplot2)
library(plotly)
library(tidyverse)

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

dep_hoeffding <- ggplot(df_long, aes(x = x, y = y, color = Effects)) +
  geom_line(size = 1) +
  scale_y_continuous(limits = c(-6, 6)) +
  labs(x = "", y = "") + # add legend
  theme_minimal() +
  theme(
    legend.position = "right",
    legend.title = element_blank(),
    legend.text = element_text(size = 10),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 11)
  )

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


# Contour plots ----

x_seq <- seq(-3, 3, length.out = 200)
grid <- expand.grid(x1 = x_seq, x2 = x_seq)
grid$f12 <- with(grid, x1 * x2 - 2 * rho * x1 - rho * x2 - rho * x1^2 - rho * x2^2 + rho)

## 1 Smooth contour plot ----
contour_plot <- ggplot(grid, aes(x = x1, y = x2, fill = f12)) +
  geom_raster(interpolate = TRUE) +  # Interpolates colors for smooth look
  stat_contour(aes(z = f12), color = "white", alpha = 0.5) +
  scale_fill_gradient2(
    low = "darkseagreen4", mid = "white", high = "salmon", midpoint = 0,
    name = "f12"
  ) +
  labs(
    x = "", y = ""
  ) +
  theme_minimal()

## 2 Line contour plot ----
ggplot(grid, aes(x = x1, y = x2, z = f12)) +
  geom_contour() + # alternative: geom_contour_filled() 
  scale_fill_brewer() +
  labs(
    x = "x1", y = "x2"
  ) +
  theme_minimal()


## 3 plotly contour plot ----
fig <- plot_ly(
  x = x_seq,
  y = x_seq,
  z = ~outer(x_seq, x_seq, FUN = "*"),  # x1 * x2
  type = "contour" 
)

fig

# Hoeffding decomposition: run `plots.R` for rho = 0.6
ggsave("../images/p_main_effect_ex1_rho06.png",
       plot = dep_hoeffding,
       width = 8, height = 6, dpi = 300, bg = "white")
ggsave("../images/p_contour_ex1_rho06.png",
       plot = contour_plot,
       width = 8, height = 6, dpi = 300, bg = "white")


