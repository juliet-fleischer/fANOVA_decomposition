# Script to automate fANOVA plot generation for various coefficient setups
# and correlation values. Requires plot_effects() from gen_fANOVA_gpt.R.

library(ggplot2)
library(tidyr)

# load plotting function
source("gen_fANOVA_gpt.R")

# ----- define scenarios -----
scenarios <- list(
  linear = list(a0 = 0, a1 = 1, a2 = 1, a11 = 0, a22 = 0, a12 = 0),
  quadratic = list(a0 = 0, a1 = 0, a2 = 0, a11 = 1, a22 = 1, a12 = 0),
  interaction = list(a0 = 0, a1 = 0, a2 = 0, a11 = 0, a22 = 0, a12 = 1),
  mixed = list(a0 = 0, a1 = 1, a2 = -1, a11 = 0.5, a22 = -0.5, a12 = 0),
  all = list(a0 = 0, a1 = 1, a2 = 1, a11 = 0.5, a22 = 0.5, a12 = 1)
)

# correlation values to explore (include negative correlations)
rhos <- seq(-1, 1, by = 0.2)

# ensure output directory exists
out_dir <- file.path("..", "images")
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# ----- generate and save plots -----
for (sc_name in names(scenarios)) {
  coeffs <- scenarios[[sc_name]]
  for (rho in rhos) {
    plots <- do.call(plot_effects, c(coeffs, rho = rho))

    rho_lab <- sprintf("%0.1f", rho)
    main_file <- file.path(out_dir, paste0(sc_name, "_rho", rho_lab, "_main.png"))
    int_file  <- file.path(out_dir, paste0(sc_name, "_rho", rho_lab, "_interaction.png"))

    ggsave(main_file, plot = plots$main, width = 6, height = 4, dpi = 300, bg = "white")
    ggsave(int_file,  plot = plots$interaction, width = 6, height = 4, dpi = 300, bg = "white")
  }
}
