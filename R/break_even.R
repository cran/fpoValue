#' Break-even throughput, membership and procurement price
#'
#' Finds the volume an FPO must handle to cover its fixed costs (and an
#' optional surplus target) given the per-unit margin between its sale
#' price and the price it pays members:
#' \deqn{Q^{*} = \frac{F + T - M_{inputs}}{p^{sale} - p^{m} - h},}
#' where \eqn{F} is fixed cost, \eqn{T} the surplus target and
#' \eqn{M_{inputs}} the margin from the input business. It also reports the
#' number of members needed at the average member volume, the margin of
#' safety at the current volume, and the highest member price the FPO can
#' pay at the current volume while breaking even, which bounds how much of
#' the aggregation gain can be passed to farmers through prices.
#'
#' All arguments are vectorised and recycled, so a grid of scenarios can be
#' evaluated in one call.
#'
#' @param fixed_cost FPO fixed operating cost for the period.
#' @param sale_price Price the FPO realises per unit.
#' @param member_price Price the FPO pays members per unit.
#' @param handling_cost FPO handling cost per unit.
#' @param input_margin Surplus from the input business for the period.
#' @param target_surplus Surplus the FPO aims to retain (for reserves or
#'   dividends).
#' @param avg_member_volume Optional average volume per member.
#' @param current_volume Optional current throughput.
#'
#' @return A data frame of class `break_even_volume` with the inputs and
#'   `unit_margin`, `break_even_volume`, `members_needed`,
#'   `margin_of_safety` (share by which current volume exceeds break-even)
#'   and `max_member_price` (at the current volume). Break-even volume is
#'   `Inf` when the unit margin is not positive and the costs are not
#'   already covered.
#'
#' @examples
#' break_even_volume(fixed_cost = 6e5, sale_price = 1650,
#'                   member_price = c(1450, 1500, 1550),
#'                   handling_cost = 60, input_margin = 1e5,
#'                   avg_member_volume = 15, current_volume = 6000)
#' @export
break_even_volume <- function(fixed_cost, sale_price, member_price,
                              handling_cost = 0, input_margin = 0,
                              target_surplus = 0, avg_member_volume = NULL,
                              current_volume = NULL) {
  .chk(fixed_cost, "fixed_cost", lower = 0, scalar = FALSE)
  .chk(sale_price, "sale_price", lower = 0, scalar = FALSE)
  .chk(member_price, "member_price", lower = 0, scalar = FALSE)
  .chk(handling_cost, "handling_cost", lower = 0, scalar = FALSE)
  .chk(input_margin, "input_margin", scalar = FALSE)
  .chk(target_surplus, "target_surplus", scalar = FALSE)
  lens <- c(length(fixed_cost), length(sale_price), length(member_price),
            length(handling_cost), length(input_margin),
            length(target_surplus))
  if (!is.null(avg_member_volume)) {
    .chk(avg_member_volume, "avg_member_volume", lower = 1e-12,
         scalar = FALSE)
    lens <- c(lens, length(avg_member_volume))
  }
  if (!is.null(current_volume)) {
    .chk(current_volume, "current_volume", lower = 0, scalar = FALSE)
    lens <- c(lens, length(current_volume))
  }
  n <- max(lens)
  if (any(n %% lens != 0)) {
    stop("Argument lengths must divide the longest length.", call. = FALSE)
  }
  rp <- function(x) rep_len(x, n)
  F <- rp(fixed_cost)
  ps <- rp(sale_price)
  pm <- rp(member_price)
  h <- rp(handling_cost)
  mi <- rp(input_margin)
  ts <- rp(target_surplus)
  need <- F + ts - mi
  um <- ps - pm - h
  bev <- ifelse(need <= 0, 0, ifelse(um > 0, need / um, Inf))
  out <- data.frame(fixed_cost = F, sale_price = ps, member_price = pm,
                    handling_cost = h, unit_margin = um,
                    break_even_volume = bev)
  if (!is.null(avg_member_volume)) {
    av <- rp(avg_member_volume)
    out$members_needed <- ifelse(is.finite(bev), ceiling(bev / av - 1e-9),
                                 Inf)
  }
  if (!is.null(current_volume)) {
    cv <- rp(current_volume)
    out$current_volume <- cv
    out$margin_of_safety <- ifelse(cv > 0, (cv - bev) / cv, NA_real_)
    out$max_member_price <- ifelse(cv > 0, ps - h - need / cv, NA_real_)
  }
  class(out) <- c("break_even_volume", "data.frame")
  out
}
