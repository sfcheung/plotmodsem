
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
#' @param expand

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

find_all_products <- function(data, expand = TRUE) {
    out <- sapply(colnames(data),
                  find_product, data = data,
                  USE.NAMES = TRUE,
                  simplify = FALSE)
    out <- out[sapply(out, function(x) !all(is.na(x)))]
    if (expand) {
        out <- expand2lower(out)
      }
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
    while (any(unlist(out) %in% names(full_list))) {
        out <- sapply(out, expand2lower_i, full_list = out)
      }
    out
  }