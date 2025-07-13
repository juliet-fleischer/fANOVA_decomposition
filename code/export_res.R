# plots for Ex. 1 g(x1, x2) = x1 + 2 * x2 + x1 * x2
ggsave("../images/p_main_effect_ex1.png",
       plot = p_main_effect,
       width = 8, height = 6, dpi = 300, bg = "transparent")
ggsave("../images/p_contour_ex1.png",
       plot = contour_plot,
       width = 8, height = 6, dpi = 300, bg = "transparent")

# run same script for rho = 0.6
ggsave("../images/p_main_effect_ex1_rho06.png",
       plot = p_main_effect,
       width = 8, height = 6, dpi = 300, bg = "transparent")
ggsave("../images/p_contour_ex1_rho06.png",
       plot = contour_plot,
       width = 8, height = 6, dpi = 300, bg = "transparent")

# plots based on zmjones fANOVA estimations
ggsave("../images/gg1_1.png",
       plot = gg1_1,
       width = 8, height = 6, dpi = 300, bg = "transparent")
ggsave("../images/gg1_2.png",
       plot = gg1_2,
       width = 8, height = 6, dpi = 300, bg = "transparent")
ggsave("../images/gg2_1.png",
       plot = gg2_1,
       width = 8, height = 6, dpi = 300, bg = "transparent")
ggsave("../images/gg2_2.png",
       plot = gg2_2,
       width = 8, height = 6, dpi = 300, bg = "transparent")
