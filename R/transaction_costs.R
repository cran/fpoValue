#' Transaction costs of a marketing channel
#'
#' Computes, for each farmer, the full cost of selling through one
#' marketing channel. Transport is trip-based, so costs per unit fall with
#' the quantity carried per trip:
#' \deqn{transport_i = trips_i \times (2\,d_i\,c_{km} + c_{trip}),\quad
#' trips_i = \lceil q_i / capacity \rceil.}
#' Other components are commission (a share of sale value), market fees
#' and handling charges per unit, the opportunity cost of the farmer's
#' time per trip, the value of produce rejected or unsold, and the
#' interest cost of delayed payment.
#'
#' Call the function once for each channel (for example individual sale in
#' a wholesale market and delivery to an FPO collection centre) and pass the
#' resulting `per_unit` costs to [member_benefit()] or
#' [collective_marketing_scenarios()].
#'
#' @param quantity Quantity sold by each farmer.
#' @param price Sale price per unit (scalar or one per farmer).
#' @param distance One-way distance (km) from farm to point of sale.
#' @param vehicle_capacity Quantity carried per trip.
#' @param cost_per_km Vehicle cost per km.
#' @param fixed_per_trip Fixed cost per trip (loading, tolls, hire minimum).
#' @param commission Commission as a share of sale value.
#' @param market_fee Market fee or levy per unit.
#' @param handling Handling, packing and loading cost per unit.
#' @param time_hours Farmer time per trip (travel, waiting, bargaining).
#' @param wage Opportunity cost of farmer time per hour.
#' @param rejection Share of quantity rejected or sold at zero value.
#' @param payment_delay Days between sale and payment.
#' @param interest_rate Annual interest rate used to value payment delay.
#' @param label Optional channel name stored with the result.
#'
#' @return A data frame of class `transaction_costs` with one row per
#'   farmer: `quantity`, `trips`, `transport`, `commission`, `fees`,
#'   `handling`, `time_cost`, `rejection_cost`, `delay_cost`, `total`,
#'   `per_unit` and `share_of_value`.
#'
#' @examples
#' q <- c(5, 12, 30, 80)
#' mandi <- transaction_costs(q, price = 1500, distance = 35,
#'                            vehicle_capacity = 10, cost_per_km = 25,
#'                            commission = 0.06, market_fee = 15,
#'                            time_hours = 8, wage = 50,
#'                            payment_delay = 7, label = "mandi")
#' centre <- transaction_costs(q, price = 1500, distance = 4,
#'                             vehicle_capacity = 10, cost_per_km = 25,
#'                             time_hours = 1.5, wage = 50,
#'                             payment_delay = 10, label = "FPO centre")
#' data.frame(quantity = q, mandi = mandi$per_unit,
#'            centre = centre$per_unit)
#' @export
transaction_costs <- function(quantity, price, distance, vehicle_capacity = 1,
                              cost_per_km = 0, fixed_per_trip = 0,
                              commission = 0, market_fee = 0, handling = 0,
                              time_hours = 0, wage = 0, rejection = 0,
                              payment_delay = 0, interest_rate = 0.12,
                              label = NULL) {
  .chk(quantity, "quantity", lower = 0, scalar = FALSE)
  n <- length(quantity)
  args <- list(price = price, distance = distance,
               cost_per_km = cost_per_km, fixed_per_trip = fixed_per_trip,
               commission = commission, market_fee = market_fee,
               handling = handling, time_hours = time_hours, wage = wage,
               payment_delay = payment_delay)
  for (nm in names(args)) .chk(args[[nm]], nm, lower = 0, scalar = FALSE)
  .chk(vehicle_capacity, "vehicle_capacity", lower = 1e-12, scalar = FALSE)
  .chk(rejection, "rejection", 0, 1, scalar = FALSE)
  .chk(interest_rate, "interest_rate", lower = 0)
  p <- .expand(price, n, "price")
  d <- .expand(distance, n, "distance")
  cap <- .expand(vehicle_capacity, n, "vehicle_capacity")
  rej <- .expand(rejection, n, "rejection")

  trips <- .trips(quantity, cap)
  transport <- trips * (2 * d * .expand(cost_per_km, n, "cost_per_km") +
                          .expand(fixed_per_trip, n, "fixed_per_trip"))
  sold <- quantity * (1 - rej)
  value <- sold * p
  comm <- value * .expand(commission, n, "commission")
  fees <- sold * .expand(market_fee, n, "market_fee")
  hand <- quantity * .expand(handling, n, "handling")
  time_cost <- trips * .expand(time_hours, n, "time_hours") *
    .expand(wage, n, "wage")
  rej_cost <- quantity * rej * p
  delay <- value * interest_rate *
    .expand(payment_delay, n, "payment_delay") / 365
  total <- transport + comm + fees + hand + time_cost + rej_cost + delay
  gross <- quantity * p
  out <- data.frame(quantity = quantity, trips = trips, transport = transport,
                    commission = comm, fees = fees, handling = hand,
                    time_cost = time_cost, rejection_cost = rej_cost,
                    delay_cost = delay, total = total,
                    per_unit = ifelse(quantity > 0, total / quantity, NA),
                    share_of_value = ifelse(gross > 0, total / gross, NA))
  class(out) <- c("transaction_costs", "data.frame")
  attr(out, "label") <- label
  out
}
