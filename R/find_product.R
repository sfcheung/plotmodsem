
#' @title One line title
#'
#' @description One line description
#'
#' @details Details
#'
#' @return
#'  Specify what are returned.
#'
#' @param data Argument description.
#' @param target Arguments to be passed to the model specific MASEM function.
#'
#' @seealso \code{\link{functionname}}
#'
#' @examples
#' \dontrun{
#' }
#' @export

find_product <- function(data, target) {
    a_col <- data[, target]
    out <- c(NA, NA)
    q <- 0
    for (i in colnames(data)) {
        if (!is.vector(data[, i], mode = "numeric")) next
        for (j in colnames(data)) {
            q <- q + 1
            if (!is.vector(data[, j], mode = "numeric")) next
            xy <- data[, i] * data[, j]
            target_xy <- all.equal(a_col, xy)
            if (isTRUE(target_xy)) {
              out <- c(i, j)
              break
              }
          }
        if (isTRUE(target_xy)) break
      }
    out
  }

#' @export

find_all_products <- function(data) {
    out <- sapply(colnames(data), find_match, data = datm,
                    USE.NAMES = TRUE,
                    simplify = FALSE)
    out
  }

expand2lower_i <- function(x, full_list) {
    to_append <- NULL
    for (y in seq_len(length(x))) {
        if (x[y] %in% names(full_list)) {
            to_append <- c(to_append, full_list[[x[y]]])
            x <- x[-y]
          }
      }
    c(x, to_append)
  }

expand2lower <- function(full_list) {
    out <- full_list
    while (any(unlist(out) %in% names(check))) {
        out <- sapply(out, find_lower_i, full_list = out)
      }
    out
  }