#' Compare individual and collective marketing scenarios
#'
#' Evaluates the same set of members under alternative marketing channels,
#' for example individual sale to a village trader, individual sale in a
#' wholesale market, FPO aggregation, FPO aggregation with grading, and an
#' FPO contract with a processor or retailer. Each scenario is described by
#' one row of `scenarios`:
#' \describe{
#'   \item{scenario}{Name.}
#'   \item{member_price}{Price received by members.}
#'   \item{member_cost}{Members' transaction cost per unit.}
#'   \item{sale_price}{Price realised by the FPO; `NA` for individual
#'     channels.}
#'   \item{handling_cost, fixed_cost, dividend_share}{Optional FPO
#'     parameters (default 0).}
#' }
#' Member net return is \eqn{q_i(p^m - c^m) + D_i}. For FPO channels the
#' FPO surplus and patronage dividends are computed as in
#' [member_benefit()]. Each scenario is compared with the `baseline`
#' scenario: the gain in members' net returns, the FPO's retained surplus,
#' the total value created relative to the baseline, the member share of
#' that value, the share of members better off, and the gain received by
#' the smallest half of suppliers.
#'
#' @param quantity Quantity marketed by each member.
#' @param scenarios Data frame of scenarios (see Details).
#' @param baseline Name of the baseline scenario; defaults to the first
#'   row.
#' @param group Optional group of each member for group-wise gains.
#'
#' @return A list of class `collective_marketing_scenarios` with `summary`
#'   (one row per scenario), `member_returns` (matrix of member net returns,
#'   members by scenarios) and, if `group` is given, `group_gain` (gain per
#'   member by group and scenario). A `plot()` method compares member gains
#'   and retained surplus across scenarios.
#'
#' @examples
#' q <- c(5, 8, 12, 20, 35, 60)
#' sc <- data.frame(
#'   scenario = c("village trader", "mandi", "FPO pooled", "FPO graded"),
#'   member_price = c(1300, 1450, 1480, 1540),
#'   member_cost = c(10, 140, 40, 55),
#'   sale_price = c(NA, NA, 1650, 1760),
#'   handling_cost = c(0, 0, 60, 90),
#'   fixed_cost = c(0, 0, 8000, 12000),
#'   dividend_share = c(0, 0, 0.5, 0.5))
#' res <- collective_marketing_scenarios(q, sc,
#'          group = c("marginal", "marginal", "small", "small",
#'                    "medium", "large"))
#' res
#' plot(res)
#' @export
collective_marketing_scenarios <- function(quantity, scenarios,
                                           baseline = NULL, group = NULL) {
  .chk(quantity, "quantity", lower = 0, scalar = FALSE)
  n <- length(quantity)
  if (!is.data.frame(scenarios) || nrow(scenarios) < 2L) {
    stop("`scenarios` must be a data frame with at least two rows.",
         call. = FALSE)
  }
  req <- c("scenario", "member_price", "member_cost", "sale_price")
  miss <- setdiff(req, names(scenarios))
  if (length(miss)) {
    stop("`scenarios` lacks column(s): ", paste(miss, collapse = ", "),
         call. = FALSE)
  }
  for (nm in c("handling_cost", "fixed_cost", "dividend_share")) {
    if (is.null(scenarios[[nm]])) scenarios[[nm]] <- 0
    scenarios[[nm]][is.na(scenarios[[nm]])] <- 0
  }
  sc_names <- as.character(scenarios$scenario)
  if (anyDuplicated(sc_names)) {
    stop("Scenario names must be unique.", call. = FALSE)
  }
  if (is.null(baseline)) baseline <- sc_names[1L]
  if (!baseline %in% sc_names) {
    stop("`baseline` must name a scenario.", call. = FALSE)
  }
  if (!is.null(group) && length(group) != n) {
    stop("`group` must have one value per member.", call. = FALSE)
  }

  k <- nrow(scenarios)
  returns <- matrix(0, n, k, dimnames = list(NULL, sc_names))
  surplus <- retained <- numeric(k)
  for (j in seq_len(k)) {
    s <- scenarios[j, ]
    .chk(s$member_price, "member_price", lower = 0)
    .chk(s$member_cost, "member_cost", lower = 0)
    base_ret <- quantity * (s$member_price - s$member_cost)
    if (is.na(s$sale_price)) {
      returns[, j] <- base_ret
    } else {
      mb <- member_benefit(quantity, outside_price = s$member_price,
                           member_price = s$member_price,
                           sale_price = s$sale_price,
                           handling_cost = s$handling_cost,
                           fixed_cost = s$fixed_cost,
                           dividend_share = s$dividend_share)
      returns[, j] <- base_ret + mb$members$dividend
      surplus[j] <- mb$fpo[["net_surplus"]]
      retained[j] <- mb$fpo[["retained"]]
    }
  }
  b <- match(baseline, sc_names)
  gain <- returns - returns[, b]
  bottom <- order(quantity)[seq_len(floor(n / 2))]
  Q <- sum(quantity)
  member_gain <- colSums(gain)
  value <- member_gain + retained - retained[b]
  summary <- data.frame(
    scenario = sc_names,
    member_net_return = colSums(returns),
    net_return_per_unit = if (Q > 0) colSums(returns) / Q else NA,
    member_gain = member_gain,
    fpo_surplus = surplus, retained = retained,
    value_created = value,
    member_share_of_value = ifelse(value > 0, member_gain / value, NA),
    share_better_off = colMeans(gain > 1e-9),
    bottom50_gain = if (length(bottom)) colSums(gain[bottom, , drop = FALSE])
    else NA,
    row.names = NULL, stringsAsFactors = FALSE)
  out <- list(summary = summary, member_returns = returns,
              baseline = baseline)
  if (!is.null(group)) {
    g <- factor(group, levels = unique(group))
    out$group_gain <- apply(gain, 2, function(x) tapply(x, g, mean))
    if (!is.matrix(out$group_gain)) {
      out$group_gain <- matrix(out$group_gain, nrow = 1,
                               dimnames = list(levels(g), sc_names))
    }
  }
  class(out) <- "collective_marketing_scenarios"
  out
}

#' @export
print.collective_marketing_scenarios <- function(x, ...) {
  cat("Collective marketing scenarios (baseline: ", x$baseline, ")\n\n",
      sep = "")
  tab <- x$summary
  for (nm in c("member_net_return", "net_return_per_unit", "member_gain",
               "fpo_surplus", "retained", "value_created", "bottom50_gain")) {
    tab[[nm]] <- .fmt(tab[[nm]])
  }
  tab$member_share_of_value <- .pct(x$summary$member_share_of_value)
  tab$share_better_off <- .pct(x$summary$share_better_off)
  print(tab, row.names = FALSE)
  if (!is.null(x$group_gain)) {
    cat("\nMean gain per member by group\n")
    print(round(x$group_gain, 2))
  }
  invisible(x)
}

#' Plot collective marketing scenarios
#'
#' Stacked bars of the gain in member net returns and the FPO's retained
#' surplus, relative to the baseline, for each scenario.
#'
#' @param x A [collective_marketing_scenarios()] object.
#' @param ... Further arguments passed to [graphics::barplot()].
#' @return `x`, invisibly.
#' @examples
#' sc <- data.frame(scenario = c("individual", "FPO"),
#'                  member_price = c(1400, 1480), member_cost = c(120, 40),
#'                  sale_price = c(NA, 1650), handling_cost = c(0, 60),
#'                  fixed_cost = c(0, 5000), dividend_share = c(0, 0.5))
#' plot(collective_marketing_scenarios(c(5, 10, 20, 40), sc))
#' @export
plot.collective_marketing_scenarios <- function(x, ...) {
  s <- x$summary
  b <- s$retained[s$scenario == x$baseline]
  m <- rbind(member_gain = s$member_gain, retained = s$retained - b)
  colnames(m) <- s$scenario
  graphics::barplot(m, beside = FALSE, col = c("steelblue", "grey70"),
                    ylab = paste0("Change relative to ", x$baseline),
                    legend.text = c("Members' gain", "FPO retained surplus"),
                    args.legend = list(x = "topleft", bty = "n"), ...)
  graphics::abline(h = 0)
  invisible(x)
}
