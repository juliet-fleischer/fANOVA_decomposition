# Script to automate fANOVA plot generation for various coefficient setups
# and correlation values. Requires plot_effects() from gen_fANOVA_gpt.R.

library(ggplot2)

# load plotting function
source(file.path("code", "gen_fANOVA_function.R"))

# helper to create file-friendly labels for coefficients
label_val <- function(v) {
  x <- sprintf("%+0.1f", v)
  x <- gsub("\\+", "p", x)
  x <- gsub("-", "m", x)
  gsub("\\.", "", x)
}

save_plots <- function(prefix, params, func_name = "y") {
  # include func_name in params list
  params$func_name <- func_name
  
  # call plot_effects dynamically
  plots <- do.call(plot_effects, params)
  
  label_parts <- mapply(
    function(name, val) paste0(name, label_val(val)),
    names(params)[names(params) != "a0" & names(params) != "func_name"],
    params[names(params) != "a0" & names(params) != "func_name"],
    SIMPLIFY = TRUE
  )
  
  suffix <- paste(label_parts, collapse = "_")
  main_file <- file.path(out_dir, paste0(prefix, "_", suffix, "_main.png"))
  int_file  <- file.path(out_dir, paste0(prefix, "_", suffix, "_interaction.png"))
  
  ggsave(main_file, plot = plots$main, width = 6, height = 4, dpi = 300, bg = "white")
  ggsave(int_file,  plot = plots$interaction, width = 6, height = 4, dpi = 300, bg = "white")
}

vary_params <- function(scenario_name, a0, a1, a2, a11, a22, a12, rho, func_name = "y") {
  # Create a list of parameters to pass to plot_effects
  params <- list(a0 = a0, a1 = a1, a2 = a2, a11 = a11, a22 = a22, a12 = a12, rho = rho)
  
  # Pass func_name along
  save_plots(scenario_name, params, func_name = func_name)
}


# ensure output directory exists in repository root
out_dir <- "images/experiment_section"
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)


# ----- Running Example -----
vary_params("running_example", a0 = 0, a1 = 1, a2 = 2, a11 = 0, a22 = 0, a12 = 1, rho = 0, func_name = "h")
vary_params("running_example", a0 = 0, a1 = 1, a2 = 2, a11 = 0, a22 = 0, a12 = 1, rho = 0.5, func_name = "h")

# ----- Linear scenario -----
vary_params("linear", a0 = 0, a1 = 1.5, a2 = 3.5, a11 = 0, a22 = 0, a12 = 0, rho = 0, func_name = "q")
vary_params("linear", a0 = 0, a1 = -2, a2 = 4, a11 = 0, a22 = 0, a12 = 0, rho = 0, func_name = "q")

# ----- Quadratic scenario -----
vary_params("quadratic", a0 = 0, a1 = 0, a2 = 0, a11 = -1, a22 = -0.4, a12 = 0, rho = 0)
vary_params("quadratic", a0 = 0, a1 = 0, a2 = 0, a11 = 2, a22 = -0.5, a12 = 0, rho = 0)

# ----- Interaction only scenario -----
rhos      <- seq(-1, 1, by = 0.5)
for (rho in rhos) {
  params <- list(a0 = 0, a1 = 0, a2 = 0, a11 = 0, a22 = 0, a12 = 2, rho = rho)
  save_plots("interaction", params, func_name = "w")
}

# ----- Mixed (linear + quadratic) scenario -----
# Select a few representative coefficient combinations instead of
# an exhaustive grid to keep the number of plots manageable.
mixed_cases <- list(
  list(a0 = 0, a1 =  2, a2 = -2, a11 =  1, a22 =  1, a12 = 0, rho = 0),
  list(a0 = 0, a1 =  2, a2 =  2, a11 = -1, a22 =  1, a12 = 0, rho = 0), 
  list(a0 = 0, a1 = -2, a2 =  2, a11 =  1, a22 = -1, a12 = 0, rho = 0), 
  list(a0 = 0, a1 = -2, a2 = -2, a11 = -1, a22 = -1, a12 = 0, rho = 0) 
)
for (params in mixed_cases) {
  save_plots("mixed", params, func_name = "p")
}

# ----- All effects scenario -----
# Select a small set of representative combinations covering
# linear, quadratic and interaction terms.
full_cases <- list(
  list(a0 = 0, a1 =  2, a2 =  0,  a11 =  1,  a22 =  0,  a12 =  0.5,  rho =  0),
  list(a0 = 0, a1 =  2, a2 =  0,  a11 =  1,  a22 =  0,  a12 =  0.5,  rho =  0.3),
  list(a0 = 0, a1 =  -2, a2 = -2, a11 =  1,  a22 = -1, a12 =  1,  rho =  0),
  list(a0 = 0, a1 =  -2, a2 = -2, a11 =  1,  a22 = -1, a12 =  1,  rho =  -0.8)
)
for (params in full_cases) {
  save_plots("full", params, func_name = "z")
}


# ----- Presentation Examples -----

vary_params("classical_ex_1", 0, 2, 0, 0, 1, 1, rho = 0, func_name = "y")
vary_params("gen_ex_1", 0, 2, 0, 0, 1, 1, rho = 0.8, func_name = "y")
