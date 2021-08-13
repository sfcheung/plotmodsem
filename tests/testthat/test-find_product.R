set.seed(897041)
n <- 100
dat <- matrix(rexp(n * 5)^2, n, 5)
mp <- .05
dat[sample(seq_len(length(dat)), round(length(dat) * mp))] <- NA
sum(is.na(dat))
head(dat)
dat <- as.data.frame(dat)

dat$V1V3 <- dat$V1 * dat$V3
dat$V4V2 <- dat$V4 * dat$V2
dat$V1V2V3 <- dat$V1 * dat$V2 * dat$V3
dat$V1V2V3V4 <- dat$V1 * dat$V2 * dat$V3 * dat$V4
dat$V4sq <- dat$V4 ^ 2

test_that("Find product term", {
  expect_equal(find_product(dat, "V1"),
              c(NA, NA))
  expect_equal(find_product(dat, "V1V3"),
              c("V1", "V3"))
  expect_equal(find_product(dat, "V4V2"),
              c("V2", "V4"))
  expect_equal(find_product(dat, "V1V2V3"),
              c("V2", "V1V3"))
})

# Add a nonnumeric column

dat$V5 <- sample(c("a", "b"), n, replace = TRUE)
head(dat)

test_that("Find product term", {
  expect_equal(find_product(dat, "V1"),
              c(NA, NA))
  expect_equal(find_product(dat, "V1V3"),
              c("V1", "V3"))
  expect_equal(find_product(dat, "V4V2"),
              c("V2", "V4"))
  expect_equal(find_product(dat, "V1V2V3"),
              c("V2", "V1V3"))
})
