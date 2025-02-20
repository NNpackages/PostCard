#' Extract information about the fitted prognostic model
#'
#' Extracts the `prognostic_info` list element from an `rctglm_prog` object. See
#' 'Value' at [rctglm_with_prognosticscore] for more details.
#'
#' @param x an object of class `rctglm_prog` (returned by
#' [rctglm_with_prognosticscore])
#'
#' @return description
#'
#' @export
#' @examples
#' # Generate some data
#' n <- 100
#' b0 <- 1
#' b1 <- 1.5
#' b2 <- 2
#' W1 <- runif(n, min = -2, max = 2)
#'
#' dat_treat <- glm_data(
#'   b0+b1*abs(sin(W1))+b2*A,
#'   W1 = W1,
#'   A = rbinom (n, 1, .5)
#' )
#'
#' dat_notreat <- glm_data(
#'   b0+b1*abs(sin(W1)),
#'   W1 = W1
#' )
#'
#' ate <- rctglm_with_prognosticscore(
#'   formula = Y ~ .,
#'   group_indicator = A,
#'   data = dat_treat,
#'   family = gaussian(),
#'   estimand_fun = "ate",
#'   data_hist = dat_notreat)
#'
#' prog(ate)
prog <- function(x) {
  UseMethod("prog")
}

#' @export
#' @rdname prog
prog.rctglm_prog <- function(x) {
  x$prognostic_info
}
