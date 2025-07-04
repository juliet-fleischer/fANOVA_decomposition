setwd("Documents/Studium_lokal/fANOVA")

# plots for Ex. 1 g(x1, x2) = x1 + 2 * x2 + x1 * x2
ggsave("images/p_main_effect_ex1.png",
       plot = p_main_effect,
       width = 8, height = 6, dpi = 300, bg = "transparent")
ggsave("images/p_contour_ex1.png",
       plot = contour_plot,
       width = 8, height = 6, dpi = 300, bg = "transparent")
