.chk <- function(x, name, lower = -Inf, upper = Inf, scalar = TRUE) {
  if (!is.numeric(x) || length(x) == 0L || anyNA(x) || any(!is.finite(x))) {
    stop(sprintf("`%s` must be finite numeric with no missing values.", name),
         call. = FALSE)
  }
  if (scalar && length(x) != 1L) {
    stop(sprintf("`%s` must be a single number.", name), call. = FALSE)
  }
  if (any(x < lower) || any(x > upper)) {
    stop(sprintf("`%s` must lie between %s and %s.", name,
                 format(lower), format(upper)), call. = FALSE)
  }
  invisible(x)
}

.expand <- function(x, n, name) {
  if (length(x) == 1L) return(rep(x, n))
  if (length(x) != n) {
    stop(sprintf("`%s` must have length 1 or %d (one value per member).",
                 name, n), call. = FALSE)
  }
  x
}

.trips <- function(q, cap) ifelse(q > 0, ceiling(q / cap - 1e-9), 0)

.gini <- function(x) {
  if (length(x) < 2L || any(x < 0) || sum(x) <= 0) return(NA_real_)
  x <- sort(x)
  n <- length(x)
  sum((2 * seq_len(n) - n - 1) * x) / (n * sum(x))
}

# Concentration index of y over the ranking by `rank_by` (ascending).
.conc_index <- function(y, rank_by) {
  n <- length(y)
  if (n < 2L || mean(y) <= 0) return(NA_real_)
  r <- (rank(rank_by, ties.method = "average") - 0.5) / n
  2 * mean((y - mean(y)) * (r - mean(r))) / mean(y)
}

.fmt <- function(x, digits = 2) {
  out <- formatC(x, format = "f", digits = digits, big.mark = ",")
  out[is.na(x)] <- "NA"
  out
}

.pct <- function(x, digits = 1) {
  out <- paste0(.fmt(100 * x, digits), "%")
  out[is.na(x)] <- "NA"
  out
}
