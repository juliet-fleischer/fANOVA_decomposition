# functional ANOVA decomposition
This repository contains scripts to visualize the functional ANOVA decomposition
of any tow-degree polynomial of the form
$y(x_1, x_1) = a_0 + a_1 x_1 + a_2 x_2 + a_{11} x_1^2 + a_{22} x_2^2 + a_{12, 11}x_1x_2$.
First, run `gen_fANOVA_function.R`. The script `run_experiments.R`uses the implemented function
to visualize the fANOVA decomposition for various example scenarios. More scenarios can be added in the same fashion.