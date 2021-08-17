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
        w_method = "percentile",
        w_percentiles = c(.25, .70),
        x_method = "percentiles",
        x_percentiles = c(.10, .95))

# Check data

fit_data <- lavInspect(fit, "data")

p_data <- layer_data(p1, 1)
a_lo <- p_data[1, "intercept"]
b_lo <- p_data[1, "slope"]
a_hi <- p_data[2, "intercept"]
b_hi <- p_data[2, "slope"]

par_t_chk <- parameterTable(fit)
# w_lo_chk <- par_t_chk[26, "est"] - sqrt(par_t_chk[18, "est"])
# w_hi_chk <- par_t_chk[26, "est"] + sqrt(par_t_chk[18, "est"])
w_lo_chk <- quantile(fit_data[, "x2"], .25)
w_hi_chk <- quantile(fit_data[, "x2"], .70)
a_lo_chk <- coef(fit)["m2~x2"] * w_lo_chk
a_hi_chk <- coef(fit)["m2~x2"] * w_hi_chk
b_lo_chk <- coef(fit)["m2~x1"] + coef(fit)["m2~x1x2"] * w_lo_chk
b_hi_chk <- coef(fit)["m2~x1"] + coef(fit)["m2~x1x2"] * w_hi_chk
chk <- c(a_lo_chk, b_lo_chk, a_hi_chk, b_hi_chk)
names(chk) <- NULL

test_that("Check lines", {
  expect_equal(c(a_lo, b_lo, a_hi, b_hi),
               chk)
})

# Check limits

p1_x_limits <- layer_scales(p1, 1)$x$limits

x_lo_chk <- quantile(fit_data[, "x1"], .10)
x_hi_chk <- quantile(fit_data[, "x1"], .95)
x_range_chk <- x_hi_chk - x_lo_chk
x_lo_limit_chk <- x_lo_chk - .1 * x_range_chk
x_hi_limit_chk <- x_hi_chk + .1 * x_range_chk
chk <- c(x_lo_limit_chk, x_hi_limit_chk)

test_that("Check limits", {
  expect_equal(p1_x_limits,
               chk)
})

# Standardized

p1_std <- plotmod(fit,
        y = "m2",
        x = "x1",
        w = "x2",
        xw = "x1x2",
        w_method = "percentile",
        w_percentiles = c(.25, .70),
        x_method = "percentiles",
        x_percentiles = c(.10, .95),
        standardized = TRUE)

p1_std
p1

p_data <- layer_data(p1_std, 1)
a_lo <- p_data[1, "intercept"]
b_lo <- p_data[1, "slope"]
a_hi <- p_data[2, "intercept"]
b_hi <- p_data[2, "slope"]

par_t_chk <- parameterTable(fit)
ov_means <- lavInspect(fit, "mean.ov")
ov_sds <- sqrt(diag(lavInspect(fit, "cov.ov")))
w_lo_chk <- (quantile(fit_data[, "x2"], .25) - ov_means["x2"]) / ov_sds["x2"]
w_hi_chk <- (quantile(fit_data[, "x2"], .70) - ov_means["x2"]) / ov_sds["x2"]
a_hi_chk <- w_hi_chk*(coef(fit)["m2~x2"] + coef(fit)["m2~x1x2"] * ov_means["x1"]) *
             ov_sds["x2"] / ov_sds["m2"]
a_lo_chk <- w_lo_chk*(coef(fit)["m2~x2"] + coef(fit)["m2~x1x2"] * ov_means["x1"]) *
             ov_sds["x2"] / ov_sds["m2"]
b_x_std <- (coef(fit)["m2~x1"] + coef(fit)["m2~x1x2"] * ov_means["x2"]) *
             ov_sds["x1"] / ov_sds["m2"]
b_w_std <- a_hi_chk
b_xw_std <- coef(fit)["m2~x1x2"] * ov_sds["x1"] * ov_sds["x2"] / ov_sds["m2"]
b_lo_chk <- b_x_std + w_lo_chk * b_xw_std
b_hi_chk <- b_x_std + w_hi_chk * b_xw_std
chk <- c(a_lo_chk, b_lo_chk, a_hi_chk, b_hi_chk)
names(chk) <- NULL

test_that("Check lines (Standardized)", {
  expect_equal(c(a_lo, b_lo, a_hi, b_hi),
               chk)
})


# Check limits

p1_std_x_limits <- layer_scales(p1_std, 1)$x$limits

par_t_chk <- parameterTable(fit)
ov_means <- lavInspect(fit, "mean.ov")
ov_sds <- sqrt(diag(lavInspect(fit, "cov.ov")))
x_lo_chk <- (quantile(fit_data[, "x1"], .10) - ov_means["x1"]) / ov_sds["x1"]
x_hi_chk <- (quantile(fit_data[, "x1"], .95) - ov_means["x1"]) / ov_sds["x1"]
x_range_chk <- x_hi_chk - x_lo_chk
x_lo_limit_chk <- x_lo_chk - .1 * x_range_chk
x_hi_limit_chk <- x_hi_chk + .1 * x_range_chk
chk <- c(x_lo_limit_chk, x_hi_limit_chk)

test_that("Check limits", {
  expect_equal(p1_std_x_limits,
               chk)
})
