#' @title probability of an outcome/outcomes.
#'
#' @param x a random variable
#'
#' @return a probability, a number between 0 and 1.
#'
#' @examples
#'
#' dice <- rv(1:6)
#' P(dice == 2)
#' @return a numeric vector of probabilities.
#'
#' @export
P <- function(x) {
  stopifnot(is.logical(x), is.rv(x))
  sum(probs(x)[x])
}
