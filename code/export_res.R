# plots for Ex. 1 g(x1, x2) = x1 + 2 * x2 + x1 * x2
ggsave("../images/p_main_effect_ex1.png",
       plot = p_main_effect,
       width = 8, height = 6, dpi = 300, bg = "white")
ggsave("../images/p_contour_ex1.png",
       plot = contour_plot,
       width = 8, height = 6, dpi = 300, bg = "white")

# Hoeffding decomposition: run `plots.R` for rho = 0.6
ggsave("../images/p_main_effect_ex1_rho06.png",
       plot = dep_hoeffding,
       width = 8, height = 6, dpi = 300, bg = "white")
ggsave("../images/p_contour_ex1_rho06.png",
       plot = contour_plot,
       width = 8, height = 6, dpi = 300, bg = "white")

# Hoeffding decomposition: run `plots.R ` for rho = 0.5
ggsave("../images/hoeffding_rho05.png",
       plot = dep_hoeffding,
       width = 8, height = 6, dpi = 300, bg = "white")

# plots based on zmjones fANOVA estimations
ggsave("../images/indep_150_main.png",
       plot = indep_150_main,
       width = 8, height = 6, dpi = 300, bg = "white")
ggsave("../images/indep_150_interact.png",
       plot = indep_150_interact,
       width = 8, height = 6, dpi = 300, bg = "white")
ggsave("../images/dep_150_main.png",
       plot = dep_150_main,
       width = 8, height = 6, dpi = 300, bg = "white")
ggsave("../images/dep_150_interact.png",
       plot = dep_150_interact,
       width = 8, height = 6, dpi = 300, bg = "white")
