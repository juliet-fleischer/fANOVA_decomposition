# Script to automate fANOVA plot generation for various coefficient setups
# and correlation values. Requires plot_effects() from gen_fANOVA_gpt.R.

library(ggplot2)

# load plotting function
source(file.path("code", "gen_fANOVA_gpt.R"))

# helper to create file-friendly labels for coefficients
label_val <- function(v) {
  x <- sprintf("%+0.1f", v)
  x <- gsub("\\+", "p", x)
  x <- gsub("-", "m", x)
  gsub("\\.", "", x)
}

save_plots <- function(prefix, params) {
  plots <- do.call(plot_effects, params)
  label_parts <- mapply(function(name, val) paste0(name, label_val(val)),
                        names(params)[names(params) != "a0"],
                        params[names(params) != "a0"],
                        SIMPLIFY = TRUE)
  suffix <- paste(label_parts, collapse = "_")
  main_file <- file.path(out_dir, paste0(prefix, "_", suffix, "_main.png"))
  int_file  <- file.path(out_dir, paste0(prefix, "_", suffix, "_interaction.png"))
  ggsave(main_file, plot = plots$main, width = 6, height = 4, dpi = 300, bg = "white")
  ggsave(int_file,  plot = plots$interaction, width = 6, height = 4, dpi = 300, bg = "white")
}

# values to explore
lin_vals  <- c(-2, 2)
quad_vals <- c(-1, 1)
int_vals  <- c(-1, 1)
rhos      <- seq(-1, 1, by = 0.2)

# ensure output directory exists in repository root
out_dir <- "images"
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# ----- Linear scenario -----
for (a1 in lin_vals) {
  for (a2 in lin_vals) {
    params <- list(a0 = 0, a1 = a1, a2 = a2, a11 = 0, a22 = 0, a12 = 0, rho = 0)
    save_plots("linear", params)
  }
}

## From this we get four cases:
# a1 = -2 and a2 = -2 --> both negative slope
# a1 = -2 and a2 = 2 --> X1 negative slope, X2 positive slope
# a1 = 2 and a2 = -2 --> X1 positive slope, X2 negative slope
# a1 = 2 and a2 = 2 --> both positive slope
# Ofc the interaction plots are not affected, are always "empty".

# ----- Quadratic scenario -----
for (a11 in quad_vals) {
  for (a22 in quad_vals) {
    params <- list(a0 = 0, a1 = 0, a2 = 0, a11 = a11, a22 = a22, a12 = 0, rho = 0)
    save_plots("quadratic", params)
  }
}

## From this we again get four cases which behave the same as the linear
# ones but with parabolic shapes:
# a11 = -1 and a22 = -1 --> both negative curvature, downward parabolas
# a11 = -1 and a22 = 1 --> X1 negative curvature, X2 positive curvature
# a11 = 1 and a22 = -1 --> X1 positive curvature, X2 negative curvature
# a11 = 1 and a22 = 1 --> both positive curvature, upward parabolas
# Ofc the interaction plots are not affected, are always "empty".

# ----- Only interaction scenario -----
for (rho in rhos) {
  params <- list(a0 = 0, a1 = 0, a2 = 0, a11 = 0, a22 = 0, a12 = 1, rho = rho)
  save_plots("interaction", params)
}

# Here we get 11 different cases based on the sequence of rho values
# rho sequence might be a bit fine-grained, would not take so many values.
# Of course the interaction plots change and what is interesting to see:
# there are three different cases for the main effect plots: rho < 0; rho = 0, rho >0
# So varying rho affects the main effects even though all main terms are set to 0.

# ----- Mixed (linear + quadratic) scenario -----
for (a1 in lin_vals) {
  for (a2 in lin_vals) {
    for (a11 in quad_vals) {
      for (a22 in quad_vals) {
        params <- list(a0 = 0, a1 = a1, a2 = a2, a11 = a11, a22 = a22, a12 = 0, rho = 0)
        save_plots("mixed", params)
      }
    }
  }
}
# Now we get all combinations of 4-D vector (a1, a2, a11, a22), i.e.
# (-2, -2, -1, -1), (-2, -2, -1, 1), (-2, -2, 1, -1), etc.
# How many combinations are this? 
# 2 (a1) * 2 (a2) * 2 (a11) * 2 (a22) = 16 combinations

# ----- All effects scenario -----
for (a1 in lin_vals) {
  for (a2 in lin_vals) {
    for (a11 in quad_vals) {
      for (a22 in quad_vals) {
        for (a12 in int_vals) {
          for (rho in rhos) {
            params <- list(a0 = 0, a1 = a1, a2 = a2, a11 = a11, a22 = a22,
                           a12 = a12, rho = rho)
            save_plots("all", params)
          }
        }
      }
    }
  }
}

