P <- function(x) {
  stopifnot(is.logical(x), is.rv(x))
  sum(probs(x)[x])
}

E <- function(x) {
  if (!is.rv(x)) stop("Input must be an rv object")
  sum(as.numeric(x) * probs(x))
}

VAR <- function(x) E((x - E(x)) ^ 2)

SD <- function(x) sqrt(VAR(x))

Z <- function(x) (x - E(x)) / SD(x)
rsim <- function(x, n) {
  stopifnot(is.rv(x))
  sample(as.vector(x), n, prob = probs(x), replace = TRUE)
}
