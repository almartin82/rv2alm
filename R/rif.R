rif <- function(x, yes, no) {
  stopifnot(is.rv(x))
  stopifnot(is.numeric(yes), length(yes) == 1)
  stopifnot(is.numeric(no), length(no) == 1)

  rv(c(no, yes), probs(x))
}
