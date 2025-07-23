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

# use coarser steps for rho to keep the grid small
rhos <- seq(-1, 1, by = 0.5)


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

# ----- Quadratic scenario -----
for (a11 in quad_vals) {
  for (a22 in quad_vals) {
    params <- list(a0 = 0, a1 = 0, a2 = 0, a11 = a11, a22 = a22, a12 = 0, rho = 0)
    save_plots("quadratic", params)
  }
}


# ----- Only interaction scenario -----
for (rho in rhos) {
  params <- list(a0 = 0, a1 = 0, a2 = 0, a11 = 0, a22 = 0, a12 = 1, rho = rho)
  save_plots("interaction", params)
}

# ----- Mixed (linear + quadratic) scenario -----
# vary a1/a2 and a11/a22 together to keep the grid manageable
for (lin in lin_vals) {
  for (quad in quad_vals) {
    params <- list(
      a0 = 0,
      a1 = lin, a2 = lin,
      a11 = quad, a22 = quad,
      a12 = 0,
      rho = 0
    )
    save_plots("mixed", params)
  }
}

# ----- All effects scenario -----
# To keep the experiment tractable, vary coefficients in pairs and
# use the coarser rho grid defined above.
for (lin in lin_vals) {
  for (quad in quad_vals) {
    for (a12 in int_vals) {
      for (rho in rhos) {
        params <- list(
          a0 = 0,
          a1 = lin, a2 = lin,
          a11 = quad, a22 = quad,
          a12 = a12,
          rho = rho
        )
        save_plots("all", params)
      }
    }
  }
}
