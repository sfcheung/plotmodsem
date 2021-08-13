library(lavaan)

dat <- sample_mod_sem

mod <- 
"
m1 ~ x1 + x2
m2 ~ x1 + x2 + x1x2
y1 ~ m1 + x1
y2 ~ m1 + m2
"
dat$x1x2 <- dat$x1 * dat$x2
fit <- sem(mod, dat, meanstructure = TRUE)

p1 <- plotmod(fit,
        y = "m2",
        x = "x1",
        w = "x2",
        xw = "x1x2")

p2 <- plotmod(fit,
        y = "m2",
        x = "x1",
        w = "x2")

test_that("Producct term not found", {
  expect_error(plotmod(fit,
        y = "y1",
        x = "m1",
        w = "m2"))
})
