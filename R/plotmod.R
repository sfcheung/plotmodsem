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
#' @param y Character. The name of the outcome variable as in the data set in `fit`.
#' @param x Character. The name of the focal variable as in the data set in `fit`.
#' @param w Character. The name of the moderator as in the data set in `fit`.
#' @param xw Charcter. The name of the product term, `x * w`. If not supplied,
#'           The function will try to find it in the data set.
#' @param x_label The label for the X-axis. Default is the vlaues of `x`.
#' @param w_label The label for the legend for the lines. Default is the value of`w`.
#' @param y_label The label for the Y-axis. Default is the value of `y`.
#' @param title The title of the graph. If not supplied, will be generated from the variable
#'               names.
#' @param a_shift Default is 0. Can be ignored for now.
#' @param expansion How much tha lower and upper limits of the axis will be adjusted.
#'
#' @examples
#' \dontrun{
#' # To be prepared
#' }
#' @export

globalVariables(c("a", "b"))

plotmod <- function(fit, y, x, w, xw,
                            x_label,
                            w_label,
                            y_label,
                            title,
                            a_shift = 0,
                            expansion = .1) {
    if (!lavaan::lavInspect(fit, "meanstructure")) {
        stop("The fitted model does no have interecepts (meanstructure).")
      }
    if (missing(x_label)) x_label <- x
    if (missing(w_label)) w_label <- w
    if (missing(y_label)) y_label <- y
    if (missing(title)) {
        title <- paste0("The Moderation Effecct of ", w,
                        " on ", x, "'s effect on ", y)
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
      }
    par_t <- lavaan::parameterEstimates(fit)
    x_sd <- par_t[par_t$lhs == x & par_t$rhs == x, "est"]
    w_sd <- par_t[par_t$lhs == w & par_t$rhs == w, "est"]
    x_mean <- par_t[par_t$lhs == x & par_t$op == "~1", "est"]
    w_mean <- par_t[par_t$lhs == w & par_t$op == "~1", "est"]
    x_lo <- x_mean - x_sd
    x_hi <- x_mean + x_sd
    w_lo <- w_mean - w_sd
    w_hi <- w_mean + w_sd
    bx <- par_t[par_t$lhs == y & par_t$rhs == x, "est"]
    bw <- par_t[par_t$lhs == y & par_t$rhs == w, "est"]
    bxw <- par_t[par_t$lhs == y & par_t$rhs == xw, "est"]
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
                    caption = "Low: 1 SD below mean; Hi: 1 SD above mean") +
      ggplot2::theme(axis.text.y = ggplot2::element_blank())
  }