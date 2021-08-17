library(lavaan)
library(ggplot2)

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
        xw = "x1x2",
        plot_x_vlines = c(.25, .50, .80),
        x_vlines_unit = "percentile")
p1

tmp <- sapply(ggplot_build(p1)$data, function(x) colnames(x)[1])
p1_data_vlines <- ggplot_build(p1)$data[tmp == "xintercept"]
p1_data_vlines_x <- sapply(p1_data_vlines, function(x) x[1, 1])

fit_data <- lavInspect(fit, "data")
chk <- quantile(fit_data[, "x1"], c(.25, .50, .80))
names(chk) <- NULL

test_that("Check vlines", {
  expect_equal(p1_data_vlines_x,
               chk)
})

# Standardized

p1_std <- plotmod(fit,
        y = "m2",
        x = "x1",
        w = "x2",
        xw = "x1x2",
        plot_x_vlines = c(.25, .50, .80),
        x_vlines_unit = "percentile",
        standardized = TRUE)

p1_std
p1

tmp <- sapply(ggplot_build(p1_std)$data, function(x) colnames(x)[1])
p1_data_vlines <- ggplot_build(p1_std)$data[tmp == "xintercept"]
p1_data_vlines_x <- sapply(p1_data_vlines, function(x) x[1, 1])

fit_data <- lavInspect(fit, "data")
chk <- quantile(fit_data[, "x1"], c(.25, .50, .80))
names(chk) <- NULL

par_t_chk <- parameterTable(fit)
ov_means <- lavInspect(fit, "mean.ov")
ov_sds <- sqrt(diag(lavInspect(fit, "cov.ov")))
chk <- (chk - ov_means["x1"]) / ov_sds["x1"]

test_that("Check vlines", {
  expect_equal(p1_data_vlines_x,
               chk)
})
