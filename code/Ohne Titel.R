# Load necessary libraries
library(ggplot2)
library(plotly)

# Define x1 and x2 range
x1 <- seq(-3, 3, length.out = 100)
x2 <- seq(-3, 3, length.out = 100)

# Main effect y1(x1) = x1
df_y1 <- data.frame(x1 = x1, y1 = x1)
p_y1 <- ggplot(df_y1, aes(x = x1, y = y1)) +
  geom_line(color = "steelblue", size = 1) +
  labs(title = "Main Effect: y1(x1)", x = "x1", y = "y1(x1)") +
  theme_minimal()

# Main effect y2(x2) = 2x2
df_y2 <- data.frame(x2 = x2, y2 = 2 * x2)
p_y2 <- ggplot(df_y2, aes(x = x2, y = y2)) +
  geom_line(color = "firebrick", size = 1) +
  labs(title = "Main Effect: y2(x2)", x = "x2", y = "y2(x2)") +
  theme_minimal()

# Interaction effect y12(x1, x2) = x1 * x2
grid <- expand.grid(x1 = x1, x2 = x2)
grid$y12 <- with(grid, x1 * x2)
z_matrix <- matrix(grid$y12, nrow = length(x1), byrow = TRUE)

# 3D surface plot using plotly
p_y12 <- plot_ly(x = ~x1, y = ~x2, z = ~z_matrix) %>%
  add_surface(colorscale = "RdBu", showscale = TRUE) %>%
  layout(title = "Interaction Effect: y12(x1, x2)",
         scene = list(xaxis = list(title = "x1"),
                      yaxis = list(title = "x2"),
                      zaxis = list(title = "y12")))

# Display plots
print(p_y1)
print(p_y2)
p_y12  # Automatically displays in viewer
