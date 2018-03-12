reverselog_trans <- function(base = exp(1)) {
  trans <- function(x) -log(x, base)
  inv <- function(x) base^(-x)
  scales::trans_new(paste0("reverselog-", format(base)), trans, inv,
                    scales::log_breaks(base = base),
                    domain = c(1e-100, Inf))
}


breaker <- function(bs) {
  10^(-seq.int(from = floor(-log10(bs[1])),
               to = ceiling(-log10(bs[2])),
               by = 1))
}


#' @title scale_x_QQ
#' @rdname scale_QQ
#'
#' @description Scale the axes to show negative log 10 of the p-values.
#'
#' @export
#'
scale_x_QQ <- function() {
  ggplot2::scale_x_continuous(trans = reverselog_trans(base = 10),
                              breaks = breaker,
                              minor_breaks = NULL,
                              labels = function(x) -log10(x))
}


#' @title scale_y_QQ
#' @rdname scale_QQ
#'
#' @export
#'
scale_y_QQ <- function() {
  ggplot2::scale_y_continuous(trans = reverselog_trans(base = 10),
                              breaks = breaker,
                              minor_breaks = NULL,
                              labels = function(x) -log10(x))
}