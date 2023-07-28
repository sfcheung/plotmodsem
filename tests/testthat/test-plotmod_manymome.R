skip_if_not_installed("lavaan")
library(lavaan)
library(manymome)

dat <- modmed_x1m3w4y1
n <- nrow(dat)
set.seed(860314)
dat$gp <- sample(c("gp1", "gp2", "gp3"), n, replace = TRUE)
dat <- cbind(dat, factor2var(dat$gp, prefix = "gp", add_rownames = FALSE))

# Categorical moderator

mod <-
"
m3 ~ m1 + x + gpgp2 + gpgp3 + x:gpgp2 + x:gpgp3
y ~ m2 + m3 + x
"
fit <- sem(mod, dat, meanstructure = TRUE, fixed.x = FALSE)
plotmod(fit,
        wlevels = list(c("gpgp2", "gpgp3")),
        y = "m3",
        x = "x")
plotmod(fit,
        wlevels = list(c("gpgp2", "gpgp3")),
        y = "m3",
        x = "x",
        graph_type = "tumble")

# Numeric moderator

dat <- modmed_x1m3w4y1
mod2 <-
"
m3 ~ m1 + x + w1 + x:w1
y ~ m3 + x
"
fit2 <- sem(mod2, dat, meanstructure = TRUE, fixed.x = FALSE)

plotmod(fit2,
        wlevels = "w1",
        y = "m3",
        x = "x")
plotmod(fit2,
        wlevels = "w1",
        y = "m3",
        x = "x",
        graph_type = "tumble")
