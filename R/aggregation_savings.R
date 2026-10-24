.tier_discount <- function(q, tiers) {
  if (is.null(tiers)) return(rep(0, length(q)))
  vapply(q, function(x) {
    ok <- tiers$min_quantity <= x
    if (any(ok)) max(tiers$discount[ok]) else 0
  }, numeric(1))
}

#' Scale economies from aggregation
#'
#' Estimates the gross savings an FPO can generate by aggregating members'
#' produce and purchases, from three sources:
#' \describe{
#'   \item{Transport consolidation}{Without the FPO, each member carries
#'     produce to the market in small vehicles. With the FPO, members carry
#'     produce to a nearby collection centre and the FPO moves the pooled
#'     volume in large vehicles. Trip-based costs make the per-unit cost of
#'     both legs depend on the quantity carried.}
#'   \item{Bulk input procurement}{A tiered discount schedule is applied to
#'     each member's own input purchase and to the pooled purchase.}
#'   \item{Grading}{Selling sorted grades instead of an ungraded lot earns a
#'     price premium per unit equal to the grade-weighted price minus the
#'     ungraded price, less any grading cost.}
#' }
#' Savings are reported in total, per unit and per member. Transport savings
#' are member-specific; the bulk leg is allocated in proportion to quantity,
#' as are grading premiums. These are *potential* savings: whether members
#' receive them depends on the FPO's pricing and dividend policy, which
#' [member_benefit()] evaluates.
#'
#' @param quantity Quantity marketed by each member.
#' @param distance_market One-way distance (km) from each farm to the
#'   market.
#' @param distance_centre One-way distance (km) from each farm to the
#'   collection centre.
#' @param centre_to_market One-way distance (km) from the collection
#'   centre to the market.
#' @param small_capacity,small_cost_per_km,small_fixed_trip Capacity, cost
#'   per km and fixed cost per trip of the small vehicles used by members.
#' @param bulk_capacity,bulk_cost_per_km,bulk_fixed_trip Capacity, cost per
#'   km and fixed cost per trip of the FPO's bulk vehicles.
#' @param input_quantity Quantity of inputs bought by each member.
#' @param input_price Undiscounted input price per unit.
#' @param discount_tiers Optional data frame with columns `min_quantity`
#'   and `discount` (share off `input_price`) defining a bulk discount
#'   schedule.
#' @param grade_shares,grade_prices Optional vectors giving the share of
#'   produce in each grade and the price of each grade.
#' @param ungraded_price Price of an ungraded lot.
#' @param grading_cost Cost of grading per unit.
#'
#' @return A list of class `aggregation_savings` with `by_source` (total
#'   and per-unit savings for transport, inputs and grading), `by_member`
#'   (savings per member) and `total`.
#'
#' @examples
#' q <- c(4, 8, 15, 25, 60)
#' aggregation_savings(q, distance_market = 30, distance_centre = 3,
#'                     centre_to_market = 30, small_capacity = 5,
#'                     small_cost_per_km = 20, bulk_capacity = 100,
#'                     bulk_cost_per_km = 45,
#'                     input_quantity = c(2, 3, 5, 8, 20), input_price = 1300,
#'                     discount_tiers = data.frame(min_quantity = c(10, 30),
#'                                                 discount = c(0.03, 0.06)),
#'                     grade_shares = c(0.5, 0.3, 0.2),
#'                     grade_prices = c(2000, 1600, 1100),
#'                     ungraded_price = 1550, grading_cost = 30)
#' @export
aggregation_savings <- function(quantity, distance_market,
                                distance_centre = 0, centre_to_market = NULL,
                                small_capacity = 1, small_cost_per_km = 0,
                                small_fixed_trip = 0, bulk_capacity = 10,
                                bulk_cost_per_km = 0, bulk_fixed_trip = 0,
                                input_quantity = 0, input_price = 0,
                                discount_tiers = NULL, grade_shares = NULL,
                                grade_prices = NULL, ungraded_price = NULL,
                                grading_cost = 0) {
  .chk(quantity, "quantity", lower = 0, scalar = FALSE)
  n <- length(quantity)
  Q <- sum(quantity)
  .chk(distance_market, "distance_market", lower = 0, scalar = FALSE)
  .chk(distance_centre, "distance_centre", lower = 0, scalar = FALSE)
  dm <- .expand(distance_market, n, "distance_market")
  dc <- .expand(distance_centre, n, "distance_centre")
  if (is.null(centre_to_market)) {
    centre_to_market <- if (Q > 0) sum(dm * quantity) / Q else mean(dm)
  }
  .chk(centre_to_market, "centre_to_market", lower = 0)
  for (nm in c("small_capacity", "bulk_capacity")) {
    .chk(get(nm), nm, lower = 1e-12)
  }
  for (nm in c("small_cost_per_km", "small_fixed_trip", "bulk_cost_per_km",
               "bulk_fixed_trip", "input_price", "grading_cost")) {
    .chk(get(nm), nm, lower = 0)
  }
  .chk(input_quantity, "input_quantity", lower = 0, scalar = FALSE)
  iq <- .expand(input_quantity, n, "input_quantity")
  if (!is.null(discount_tiers)) {
    if (!is.data.frame(discount_tiers) ||
        !all(c("min_quantity", "discount") %in% names(discount_tiers))) {
      stop("`discount_tiers` must be a data frame with columns ",
           "`min_quantity` and `discount`.", call. = FALSE)
    }
    .chk(discount_tiers$min_quantity, "discount_tiers$min_quantity",
         lower = 0, scalar = FALSE)
    .chk(discount_tiers$discount, "discount_tiers$discount", 0, 1,
         scalar = FALSE)
  }

  # Transport
  ind <- .trips(quantity, small_capacity) *
    (2 * dm * small_cost_per_km + small_fixed_trip)
  leg1 <- .trips(quantity, small_capacity) *
    (2 * dc * small_cost_per_km + small_fixed_trip)
  bulk_total <- .trips(Q, bulk_capacity) *
    (2 * centre_to_market * bulk_cost_per_km + bulk_fixed_trip)
  leg2 <- if (Q > 0) bulk_total * quantity / Q else rep(0, n)
  transport_saving <- ind - leg1 - leg2

  # Inputs
  d_ind <- .tier_discount(iq, discount_tiers)
  d_col <- .tier_discount(sum(iq), discount_tiers)
  input_saving <- iq * input_price * (d_col - d_ind)

  # Grading
  if (!is.null(grade_shares) || !is.null(grade_prices)) {
    if (is.null(grade_shares) || is.null(grade_prices) ||
        is.null(ungraded_price)) {
      stop("Grading needs `grade_shares`, `grade_prices` and ",
           "`ungraded_price`.", call. = FALSE)
    }
    .chk(grade_shares, "grade_shares", 0, 1, scalar = FALSE)
    .chk(grade_prices, "grade_prices", lower = 0, scalar = FALSE)
    .chk(ungraded_price, "ungraded_price", lower = 0)
    if (length(grade_shares) != length(grade_prices)) {
      stop("`grade_shares` and `grade_prices` must have equal length.",
           call. = FALSE)
    }
    if (abs(sum(grade_shares) - 1) > 1e-6) {
      stop("`grade_shares` must sum to 1.", call. = FALSE)
    }
    premium <- sum(grade_shares * grade_prices) - ungraded_price -
      grading_cost
  } else {
    premium <- 0
  }
  grading_saving <- quantity * premium

  by_member <- data.frame(member = seq_len(n), quantity = quantity,
                          transport_individual = ind,
                          transport_collective = leg1 + leg2,
                          transport_saving = transport_saving,
                          input_saving = input_saving,
                          grading_saving = grading_saving,
                          total_saving = transport_saving + input_saving +
                            grading_saving)
  tot <- c(transport = sum(transport_saving), inputs = sum(input_saving),
           grading = sum(grading_saving))
  by_source <- data.frame(source = names(tot), total = as.numeric(tot),
                          per_unit_marketed = if (Q > 0) tot / Q else NA,
                          share = if (sum(tot) != 0) tot / sum(tot) else NA,
                          row.names = NULL)
  out <- list(by_source = by_source, by_member = by_member,
              total = sum(tot), per_unit = if (Q > 0) sum(tot) / Q else NA,
              pooled_input_discount = d_col,
              grading_premium_per_unit = premium)
  class(out) <- "aggregation_savings"
  out
}

#' @export
print.aggregation_savings <- function(x, ...) {
  cat("Scale economies from aggregation\n\n")
  tab <- x$by_source
  tab$total <- .fmt(tab$total)
  tab$per_unit_marketed <- .fmt(tab$per_unit_marketed)
  tab$share <- .pct(x$by_source$share)
  print(tab, row.names = FALSE)
  cat("\nTotal potential saving: ", .fmt(x$total), " (", .fmt(x$per_unit),
      " per unit marketed)\n", sep = "")
  neg <- sum(x$by_member$total_saving < 0)
  if (neg > 0) {
    cat("Members with negative potential saving: ", neg, "\n", sep = "")
  }
  invisible(x)
}
