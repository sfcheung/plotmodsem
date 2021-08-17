#' @title Plot the moderation effect in a path model
#'
#' @description Plot the moderation effect in a path model
#'
#' @details This function extracts the information stored
#'          in the `lavaan` fit object to plot a two-line
#'          graph, one for the relation between the focal variale (`x`) and
#'          the outcome variable (`y`) when the moderator (`w`) is one stanard
#'          deviation below mean, and one when the moderator is one standard
#'          deviation above mean.
#'
#' @return
#'  A [ggplot2] graph.
#'
#' @param fit The output from [lavaan::lavaan] or its wrapper, such as
#'            [lavaan::sem].
#' @param y The name of the outcome variable as in the data set in `fit`. It
#'          can be the name of the variable, with or without quotes. 
#' @param x The name of the focal variable as in the data set in
#'           It
#'          can be the name of the variable, with or without quotes.`fit`.
#' @param w The name of the moderator as in the data set in `fit`.
#'           It
#'          can be the name of the variable, with or without quotes.
#' @param xw The name of the product term, `x * w`. If not supplied,
#'           The function will try to find it in the data set.
#'             It
#'          can be the name of the variable, with or without quotes.
#' @param x_label The label for the X-axis. Default is the vlaues of `x`.
#' @param w_label The label for the legend for the lines. Default is the value of`w`.
#' @param y_label The label for the Y-axis. Default is the value of `y`.
#' @param title The title of the graph. If not supplied, will be generated from the variable
#'               names.
#' @param a_shift Default is 0. Can be ignored for now.
#' @param expansion How much tha lower and upper limits of the axis will be adjusted.
#' @param standardized Logical. Plot the moderation effect in standardized metric. All three
#'                     variables, `x`, `w`, and `y` will be standardized. Default
#'                     is `FALSE`
#' @param digits Number of decimal digits to print. Default is 3.
#'
#' @examples
#' \dontrun{
#' # To be prepared
#' }
#' @export

plotmod <- function(fit, y, x, w, xw,
                            x_label,
                            w_label,
                            y_label,
                            title,
                            a_shift = 0,
                            expansion = .1,
                            standardized = FALSE,
                            digits = 3
                    ) {
    if (!lavaan::lavInspect(fit, "meanstructure")) {
        stop("The fitted model does no have interecepts (meanstructure).")
      }
    x0 <- deparse(substitute(x))
    if (inherits(tryCatch(x00 <- as.character(x), error = function(e) e),
                 "simpleError")) {
        x <- x0
      } else {
        x <- x00
      }
    w0 <- deparse(substitute(w))
    if (inherits(tryCatch(w00 <- as.character(w), error = function(e) e),
                 "simpleError")) {
        w <- w0
      } else {
        w <- w00
      }
    y0 <- deparse(substitute(y))
    if (inherits(tryCatch(y00 <- as.character(y), error = function(e) e),
                 "simpleError")) {
        y <- y0
      } else {
        y <- y00
      }
    if (!is.character(y)) y <- y0
    if (missing(x_label)) x_label <- x
    if (missing(w_label)) w_label <- w
    if (missing(y_label)) y_label <- y
    if (missing(title)) {
        if (standardized) {
            title <- paste0("The Moderation Effect of ", w,
                            " on ", x, "'s effect on ", y,
                            " (Standardized)")
          } else {
            title <- paste0("The Moderation Effect of ", w,
                            " on ", x, "'s effect on ", y)
          }
      }
    if (missing(xw)) {
        all_prods <- find_all_products(lavaan::lavInspect(fit, "data"))
        tmp <- sapply(all_prods, function(a) {
                          if (length(a) != 2) {
                              return(FALSE)
                            } else {
                              if (all(c(x, w) %in% a)) {
                                  return(TRUE)
                                } else {
                                  return(FALSE)
                                }
                            }
                        })
        if (all(!tmp)) {
            stop("xw was not supplied but the product term could be found.")
          }
        if (sum(tmp) != 1) {
            stop("xw was not supplied but more than one possible product term was found.")
          }
        xw <- names(tmp[tmp])
      } else {
        xw0 <- deparse(substitute(xw))
        if (inherits(tryCatch(xw00 <- as.character(xw), error = function(e) e),
                    "simpleError")) {
            xw <- xw0
          } else {
            xw <- xw00
          }
      }
    par_t <- lavaan::parameterEstimates(fit)
    x_sd_raw <- sqrt(lavaan::lavInspect(fit, "cov.ov")[x, x])
    w_sd_raw <- sqrt(lavaan::lavInspect(fit, "cov.ov")[w, w])
    y_sd_raw <- sqrt(lavaan::lavInspect(fit, "cov.ov")[y, y])
    x_mean_raw <- lavaan::lavInspect(fit, "mean.ov")[x]
    w_mean_raw <- lavaan::lavInspect(fit, "mean.ov")[w]
    bx_raw <- par_t[par_t$lhs == y & par_t$rhs == x, "est"]
    bw_raw <- par_t[par_t$lhs == y & par_t$rhs == w, "est"]
    bxw_raw <- par_t[par_t$lhs == y & par_t$rhs == xw, "est"]
    if (standardized) {
        std_t <- lavaan::standardizedSolution(fit)
        x_sd <- 1
        w_sd <- 1
        x_mean <- 0
        w_mean <- 0
        bx <- (bx_raw + bxw_raw * w_mean_raw) * x_sd_raw / y_sd_raw
        bw <- (bw_raw + bxw_raw * x_mean_raw) * w_sd_raw / y_sd_raw
        bxw <- par_t[par_t$lhs == y & par_t$rhs == xw, "est"] *
                      x_sd_raw * w_sd_raw / y_sd_raw
      } else {
        x_sd <- x_sd_raw
        w_sd <- w_sd_raw
        x_mean <- x_mean_raw
        w_mean <- w_mean_raw
        bx <- bx_raw
        bw <- bw_raw
        bxw <- bxw_raw
      }
    x_lo <- x_mean - x_sd
    x_hi <- x_mean + x_sd
    w_lo <- w_mean - w_sd
    w_hi <- w_mean + w_sd
    dat_plot <- data.frame(w = c("Low", "High"),
                           a = c(a_shift + bw * w_lo,
                                 a_shift + bw * w_hi),
                           b = c(bx + bxw * w_lo,
                                 bx + bxw * w_hi),
                           stringsAsFactors = FALSE)
    y_x_lo_w_lo <- a_shift + bx * x_lo + bw * w_lo + bxw * x_lo * w_lo
    y_x_lo_w_hi <- a_shift + bx * x_lo + bw * w_hi + bxw * x_lo * w_hi
    y_x_hi_w_lo <- a_shift + bx * x_hi + bw * w_lo + bxw * x_hi * w_lo
    y_x_hi_w_hi <- a_shift + bx * x_hi + bw * w_hi + bxw * x_hi * w_hi
    y_min <- min(y_x_lo_w_lo, y_x_lo_w_hi, y_x_hi_w_lo, y_x_hi_w_hi)
    y_max <- max(y_x_lo_w_lo, y_x_lo_w_hi, y_x_hi_w_lo, y_x_hi_w_hi)
    y_range <- y_max - y_min
    x_range <- x_hi - x_lo
    b_format <- paste0("%.", digits, "f")
    subtxt <- paste0(w_label, " low: ", x_label, " effect = ",
                     sprintf(b_format,
                             dat_plot[dat_plot$w == "Low", "b"]),
                     "; ",
                     w_label, " high: ", x_label, " effect = ",
                     sprintf(b_format,
                             dat_plot[dat_plot$w == "High", "b"])
                     )
    ggplot2::ggplot() +
      ggplot2::scale_x_continuous(name = x_label,
                                  limits = c(x_lo - expansion * x_range,
                                             x_hi + expansion * x_range)) +
      ggplot2::scale_y_continuous(name = y_label,
                                  limits = c(y_min - expansion * y_range,
                                             y_max + expansion * y_range)) +
      ggplot2::scale_linetype(name = w_label) +
      ggplot2::geom_abline(data = dat_plot,
                  mapping = ggplot2::aes(slope = b,
                                         intercept = a,
                                         linetype = factor(w)),
                                         size = 1) +
      ggplot2::labs(title = title,
                    subtitle = subtxt,
                    caption = "Low: 1 SD below mean; Hi: 1 SD above mean") +
      ggplot2::theme(axis.text.y = ggplot2::element_blank())
  }
