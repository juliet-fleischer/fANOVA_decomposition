library(ggplot2)
library(tidyr)

plot_effects <- function(a0, a1, a2, a11, a22, a12, rho,
                         xlim = c(-3, 3), n = 200) {
  # ---- Corrected coefficients ----
  c_int <- -a12
  c12   <- a11 + (rho / (1 + rho^2)) * a12
  c22   <- a22 + (rho / (1 + rho^2)) * a12
  c11   <- a1
  c21   <- a2
  c0    <- a0 + a11 + a22 + (rho * (3 - rho^2) / (1 + rho^2)) * a12
  
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
  
  p1 <- ggplot(df_main, aes(x = x, y = Effect, color = Variable)) +
    geom_line(size = 1) +
    labs(title = "Main Effects of X₁ and X₂",
         x = "x", y = "Effect") +
    theme_minimal()
  
  # ---- Interaction effect ----
  grid <- expand.grid(x1 = x, x2 = x)
  psi_int <- function(x1, x2) {
    rho * (x1^2 + x2^2) / (1 + rho^2) - x1 * x2 +
      rho * (rho^2 - 1) / (1 + rho^2)
  }
  grid$Interaction <- c_int * psi_int(grid$x1, grid$x2)
  
  p2 <- ggplot(grid, aes(x = x1, y = x2, z = Interaction)) +
    geom_contour_filled() +
    labs(title = "Interaction Effect",
         x = expression(x[1]), y = expression(x[2])) +
    theme_minimal()
  
  list(
    main = p1,
    interaction = p2
  )
}






