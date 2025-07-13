# install.packages("remotes")
# remotes::install_github("zmjones/mmpf/pkg")
# remotes::install_github("zmjones/fanova")
library(fanova)
library(randomForest)
library(ggplot2)
library(data.table)

# Functions ----
a = 0
g <- function(x1, x2) {
  a + x1 + 2 * x2 + x1 * x2
}


# Example 1 ----
## known model g(x1, x2) = x1 + 2 * x2 + x1 * x2
## Independent inputs 
# simulate standard normal inputs, i.e. x1, x2 ~ N(0, 1)
set.seed(789)
x1 = rnorm(100)
x2 = rnorm(100)
y = g(x1, x2)
df = data.frame(x1 = x1, x2 = x2, y = y)

g.features = df[, c("x1", "x2")]
g.target = df$y

m = randomForest(g.features, g.target)

fa = functionalANOVA(g.features, c("x1", "x2"), c(150, 2), m)
print(fa)

plt = melt(fa[fa$effect %in% c("x1", "x2"), ],
           id.vars = c("f", "effect"), na.rm = TRUE)
ggplot(plt, aes(value, f)) +
  geom_point() + geom_line() + facet_wrap(~ variable, scales = "free_x")

ggplot(fa[fa$effect == "x1:x2", ], aes(x1, x2, z = f, fill = f)) + geom_raster()

# not really centred around zero?
# also the interaction effects are kind of "shifted"?


## Question ----
# instead of random forest fit a linear model with interaction
m1 = lm(y ~ x1 * x2, data = df)
fa1 = functionalANOVA(g.features, c("x1", "x2"), c(100, 2), m1)
print(fa1)

plt1 = melt(fa1[fa1$effect %in% c("x1", "x2"), ],
            id.vars = c("f", "effect"), na.rm = TRUE)
ggplot(plt1, aes(value, f)) +
  geom_point() + geom_line() + facet_wrap(~ variable, scales = "free_x")
ggplot(fa1[fa1$effect == "x1:x2", ], aes(x1, x2, z = f, fill = f)) + geom_raster()
