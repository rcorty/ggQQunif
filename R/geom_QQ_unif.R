#' geom_QQ_unif
#'
#' @param keep_first_n Keep the first n p-values (closest to 0).
#' @param step How many points to plot with each doubling, e.g. between 1/8 and 1/4, between 1/4 and 1/2, etc.
#' @param sorted logical. Are the p-values already sorted?
#' @param mapping Default list of aesthetic mappings to use for plot. If not specified, must be suppled in each layer added to the plot.
#' @param data Default dataset to use for plot. If not already a data.frame, will be converted to one by fortify. If not specified, must be suppled in each layer added to the plot.
#' @param geom Specify which geom to plot -- 'point' and 'line' are the two choices that make sense.
#' @param position Position adjustment, either as a string, or the result of a call to a position adjustment function.
#' @param show.legend logical. Should this layer be included in the legends? NA, the default, includes if any aesthetics are mapped. FALSE never includes, and TRUE always includes.
#' @param inherit.aes If FALSE, overrides the default aesthetics, rather than combining with them. This is most useful for helper functions that define both data and aesthetics and shouldn't inherit behaviour from the default plot specification.
#'
#' @return a ggplot
#' @export
#'
#' @examples
#' library(ggplot2)
#' library(dplyr)
#'
#' set.seed(27599)
#' d <- data.frame(s = runif(n = 5e5))
#'
#' d %>%
#' ggplot(mapping = aes(sample = s)) +
#'  geom_QQ_unif() +
#'  scale_x_QQ() +
#'  scale_y_QQ() +
#'  theme_minimal()
#'
geom_QQ_unif <- function(mapping = NULL,
                         data = NULL,
                         geom = "point",
                         position = "identity",
                         show.legend = NA,
                         inherit.aes = TRUE,
                         keep_first_n = 1024,
                         step = 8,
                         sorted = FALSE) {
  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = QQ_unif,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(keep_first_n = keep_first_n,
                  step = step,
                  sorted = sorted)
  )
}

stat_QQ_unif <- geom_QQ_unif

QQ_unif <- ggplot2::ggproto(
  "StatQq",
  ggplot2::Stat,
  default_aes = ggplot2::aes(y = ..sample.., x = ..theoretical..),

  required_aes = c("sample"),

  compute_group = function(data,
                           scales,
                           keep_first_n = 1024,
                           step = 8,
                           sorted = FALSE) {

    sample <- if (sorted) { data$sample } else { sort(data$sample) }

    n <- length(sample)

    idxs <- sift(n = n, step = step, keep_first_n = keep_first_n)

    data.frame(theoretical = (idxs - 0.5)/n,
               sample = sample[idxs])
  }

)


sift <- function(n, step, keep_first_n) {

  if (n <= keep_first_n) {

    return(1:n)

  } else {

    half <- round(n/2)
    return(c(sift(n = half, step = step, keep_first_n = keep_first_n),
             seq.int(from = half + half/step,
                     to = n,
                     by = round(half/step))))

  }

}