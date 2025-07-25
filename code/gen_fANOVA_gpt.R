library(ggplot2)
library(tidyr)

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


plot_effects <- function(a0, a1, a2, a11, a22, a12, rho,
                         xlim = c(-2, 2), n = 200) {
  # ---- Corrected coefficients ----
  c_int <- -a12
  c12   <- a11 + (rho / (1 + rho^2)) * a12
  c22   <- a22 + (rho / (1 + rho^2)) * a12
  c11   <- a1
  c21   <- a2
  c0    <- a0 + a11 + a22 + rho * a12
  
  # ---- Main effects ----
  x <- seq(xlim[1], xlim[2], length.out = n)
  df_main <- data.frame(
    x = x,
    Effect1 = c11 * x + c12 * (x^2 - 1),
    Effect2 = c21 * x + c22 * (x^2 - 1)
  ) %>%
    pivot_longer(cols = c(Effect1, Effect2),
                 names_to = "Variable",
                 values_to = "Effect")

  # main_colors <- c("Effect1" = "#FDBFBF",  # soft coral-peach
  #                  "Effect2" = "#A6CEE3")  # pastel blue
  # main_colors <- c(
  #   "Effect1" = "#F4A582",  # soft coral-rose
  #   "Effect2" = "#92C5DE"   # soft sky blue
  # )
  main_colors <- c(
    "Effect1" = "#FDD9A0",  # light peach
    "Effect2" = "#C2A5CF"   # dusty lavender
  )

  p1 <- ggplot(df_main, aes(x = x, y = Effect, color = Variable)) +
    geom_line(size = 2) +
    labs(x = expression(X[i]), y = "Effect", color = "fANOVA term") +
    scale_color_manual(values = main_colors,
                       labels = c(expression(y[1]),
                                  expression(y[2]))) +
    theme_pub
  
  # ---- Interaction effect ----
  grid <- expand.grid(x1 = x, x2 = x)
  psi_int <- function(x1, x2) {
    rho * (x1^2 + x2^2) / (1 + rho^2) - x1 * x2 +
      rho * (rho^2 - 1) / (1 + rho^2)
  }
  grid$Interaction <- c_int * psi_int(grid$x1, grid$x2)
  
  p2 <- ggplot(grid, aes(x = x1, y = x2, z = Interaction)) +
    geom_contour_filled() +
    scale_fill_brewer(palette = "YlGn") +
    labs(x = expression(X[1]), y = expression(X[2]), fill = "Interaction") +
    theme_pub
  
  list(
    main = p1,
    interaction = p2
  )
}


# plot_effects(0, -1, 5, 0, 0, -4, rho = 0)


