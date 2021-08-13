# Generate data

library(MASS)
library(lavaan)
n <- 500
mod <- 
"
m1 ~ x1 + x2
m2 ~ x1 + x2 + x1x2
y1 ~ m1 + x1
y2 ~ m1 + m2
"

set.seed(954745)
xs <- mvrnorm(n, c(x1 = 2, x2 = 3),
              matrix(c(1, .2, .2, 1), 2, 2))
x1 <- xs[, "x1"]
x2 <- xs[, "x2"]
m1 <- 2 + .2 * x1 + .3 * x2 + rnorm(n, 0, 1)
m2 <- 3 + .2 * x1 + .3 * x2 + .1 * x1 * x2 + rnorm(n, 0, 1)
y1 <- 4 + .3 * m1 + .4 * x1 + rnorm(n, 0, 1)
y2 <- 5 + .4 * m1 - .3 * m2 + rnorm(n, 0, .7)
dat <- data.frame(x1, x2, m1, m2, y1, y2)
summary(lm_m1 <- lm(m1 ~ x1 + x2, dat))
summary(lm_m2 <- lm(m2 ~ x1 * x2, dat))
summary(lm_y1 <- lm(y1 ~ m1 + x1, dat))
summary(lm_y2 <- lm(y2 ~ m1 + m2, dat))

dat_tmp <- dat
dat_tmp$x1x2 <- dat$x1 * dat$x2
#dat_tmp$m1m2 <- dat$m1 * dat$m2
fit <- sem(mod, dat_tmp, meanstructure = TRUE, fixed.x = FALSE)
summary(fit, fit.measures = TRUE)


sample_mod_sem <- dat
usethis::use_data(sample_mod_sem, overwrite = TRUE)
