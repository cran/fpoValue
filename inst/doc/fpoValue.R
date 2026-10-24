## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>", fig.width = 6,
                      fig.height = 4)

## -----------------------------------------------------------------------------
library(fpoValue)

## -----------------------------------------------------------------------------
set.seed(11)
n <- 80
size <- sample(c("marginal", "small", "medium", "large"), n, replace = TRUE,
               prob = c(0.45, 0.30, 0.17, 0.08))
q <- round(c(marginal = 6, small = 14, medium = 30, large = 70)[size] *
             stats::rlnorm(n, 0, 0.3), 1)
dist_market <- round(stats::runif(n, 15, 35))
dist_centre <- round(stats::runif(n, 1, 6))
table(size)

## -----------------------------------------------------------------------------
mandi <- transaction_costs(q, price = 1500, distance = dist_market,
                           vehicle_capacity = 10, cost_per_km = 12,
                           fixed_per_trip = 80, commission = 0.04,
                           market_fee = 15, time_hours = 6, wage = 45,
                           rejection = 0.02, payment_delay = 3)
centre <- transaction_costs(q, price = 1500, distance = dist_centre,
                            vehicle_capacity = 10, cost_per_km = 12,
                            time_hours = 1.5, wage = 45,
                            payment_delay = 12)
tapply(mandi$per_unit, size, mean)
tapply(centre$per_unit, size, mean)

## -----------------------------------------------------------------------------
agg <- aggregation_savings(q, distance_market = dist_market,
                           distance_centre = dist_centre,
                           small_capacity = 10, small_cost_per_km = 12,
                           small_fixed_trip = 80, bulk_capacity = 90,
                           bulk_cost_per_km = 48, bulk_fixed_trip = 600,
                           input_quantity = q / 6, input_price = 1350,
                           discount_tiers = data.frame(
                             min_quantity = c(20, 100, 300),
                             discount = c(0.02, 0.05, 0.08)),
                           grade_shares = c(0.45, 0.35, 0.20),
                           grade_prices = c(1850, 1550, 1100),
                           ungraded_price = 1500, grading_cost = 25)
agg

## -----------------------------------------------------------------------------
mb <- member_benefit(q, outside_price = 1500, member_price = 1540,
                     sale_price = 1660, outside_cost = mandi$per_unit,
                     member_cost = centre$per_unit, handling_cost = 55,
                     fixed_cost = 30000, input_quantity = q / 6,
                     input_market_price = 1350, input_member_price = 1310,
                     input_bulk_price = 1260, dividend_share = 0.5,
                     group = size)
mb
mb$distribution$by_group

## -----------------------------------------------------------------------------
policy <- expand.grid(member_price = c(1520, 1540, 1560),
                      dividend_share = c(0, 0.5, 1))
policy$member_share <- mapply(function(p, d) {
  member_benefit(q, 1500, p, 1660, outside_cost = mandi$per_unit,
                 member_cost = centre$per_unit, handling_cost = 55,
                 fixed_cost = 30000, dividend_share = d)$value[["member_share"]]
}, policy$member_price, policy$dividend_share)
xtabs(round(member_share, 3) ~ member_price + dividend_share, policy)

## -----------------------------------------------------------------------------
break_even_volume(fixed_cost = 30000, sale_price = 1660,
                  member_price = c(1520, 1540, 1560), handling_cost = 55,
                  input_margin = sum(q / 6) * 50,
                  avg_member_volume = mean(q), current_volume = sum(q))

## -----------------------------------------------------------------------------
sc <- data.frame(
  scenario = c("village trader", "individual mandi", "FPO pooled",
               "FPO graded", "FPO-processor contract"),
  member_price = c(1380, 1500, 1520, 1540, 1580),
  member_cost = c(mean(centre$per_unit), mean(mandi$per_unit),
                  mean(centre$per_unit), mean(centre$per_unit) + 10,
                  mean(centre$per_unit) + 10),
  sale_price = c(NA, NA, 1620, 1660, 1720),
  handling_cost = c(0, 0, 45, 55, 70),
  fixed_cost = c(0, 0, 20000, 30000, 40000),
  dividend_share = c(0, 0, 0.5, 0.5, 0.5))
res <- collective_marketing_scenarios(q, sc, baseline = "individual mandi",
                                      group = size)
res
plot(res, las = 2, cex.names = 0.7)

