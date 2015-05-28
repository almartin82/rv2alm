rv <- function(x, probs = NULL) {
  if (is.rv(x)) x <- as.numeric(x)
  if (is.null(probs)) {
    probs <- rep(1, length(x)) / length(x)
  } else {
    if (any(probs < 0)) stop("Probabilities must be positive")
    if (abs(sum(probs) - 1) > 1e-6) stop("Probabilities must sum to 1")
  }

  # Simplify by summing probabilities with equal x's. Need to use
  # addNA since otherwise tapply silently drops groups with missing values
  grp <- addNA(x, ifany = TRUE)
  x_new <- as.vector(tapply(x, grp, "[", 1))
  probs <- as.vector(tapply(probs, grp, sum))

  # Set probs and class attributes
  structure(x_new, probs = probs, class = "rv")
}

is.rv <- function(x) inherits(x, "rv")

as.rv <- function(x) UseMethod("as.rv")

as.rv.rv <- function(x) x

as.rv.numeric <- function(x) rv(x)
as.rv.integer <- function(x) rv(x)
as.rv.logical <- function(x) rv(x)
as.rv.character <- function(x) rv(x)


probs <- function(x) attr(x, "probs")

print.rv <- function(x, ...) {
  X <- format(x, digits = 3)
  P <- format(probs(x), digits = 3)
  out <- cbind(X = X, "P(X)" = P)
  rownames(out) <- rep("", nrow(out))
  print(out, quote = FALSE)
}

"[.rv" <- function(x, i, ...) {
  rv(as.numeric(x)[i], prop.table(probs(x)[i]))
}

abs.rv <- function(x) {
  rv(NextMethod(), probs(x))
}

log.rv <- function(x, base = exp(1)) {
  rv(NextMethod(), probs(x))
}

exp.rv <- function(x) {
  rv(NextMethod(), probs(x))
}

sqrt.rv <- function(x) {
  rv(NextMethod(), probs(x))
}

plot.rv <- function(x, ...) {
  name <- deparse(substitute(x))
  ylim <- range(0, probs(x))

  plot(as.numeric(x), probs(x), type = "h", ylim = ylim,
    xlab = name, ylab = paste0("P(", name, ")"), ...)
  points(as.numeric(x), probs(x), pch = 20)
  abline(h = 0, col = "gray")
}
