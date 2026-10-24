test_that("transaction_costs reproduces hand calculation", {
  tc <- transaction_costs(quantity = c(0, 15), price = 100, distance = 10,
                          vehicle_capacity = 10, cost_per_km = 2,
                          fixed_per_trip = 5, commission = 0.05,
                          market_fee = 1, handling = 0.5, time_hours = 2,
                          wage = 10, rejection = 0.1, payment_delay = 365,
                          interest_rate = 0.1)
  expect_s3_class(tc, "transaction_costs")
  expect_equal(tc$trips, c(0, 2))
  expect_equal(tc$transport[2], 2 * (2 * 10 * 2 + 5))
  sold <- 15 * 0.9
  expect_equal(tc$commission[2], sold * 100 * 0.05)
  expect_equal(tc$fees[2], sold)
  expect_equal(tc$rejection_cost[2], 1.5 * 100)
  expect_equal(tc$delay_cost[2], sold * 100 * 0.1)
  expect_true(is.na(tc$per_unit[1]))
  expect_equal(tc$total[1], 0)
})

test_that("per-unit transport cost falls with volume", {
  tc <- transaction_costs(c(2, 10, 50), 100, 20, vehicle_capacity = 10,
                          cost_per_km = 5)
  expect_true(all(diff(tc$per_unit) <= 0))
  expect_error(transaction_costs(-1, 100, 10))
  expect_error(transaction_costs(1, 100, 10, rejection = 2))
})

test_that("aggregation_savings components add up", {
  q <- c(4, 8, 15, 25, 60)
  tiers <- data.frame(min_quantity = c(10, 30), discount = c(0.03, 0.06))
  a <- aggregation_savings(q, distance_market = 30, distance_centre = 3,
                           centre_to_market = 30, small_capacity = 5,
                           small_cost_per_km = 20, bulk_capacity = 100,
                           bulk_cost_per_km = 45,
                           input_quantity = c(2, 3, 5, 8, 20),
                           input_price = 1000, discount_tiers = tiers,
                           grade_shares = c(0.5, 0.5),
                           grade_prices = c(2000, 1000),
                           ungraded_price = 1400, grading_cost = 20)
  expect_s3_class(a, "aggregation_savings")
  expect_equal(a$total, sum(a$by_member$total_saving))
  expect_equal(a$pooled_input_discount, 0.06)
  expect_equal(a$grading_premium_per_unit, 1500 - 1400 - 20)
  # member 5 buys 20 units: own discount 3%, pooled 6%
  expect_equal(a$by_member$input_saving[5], 20 * 1000 * 0.03)
  expect_equal(a$by_member$transport_individual[1], 1 * 2 * 30 * 20)
  bulk <- 2 * (2 * 30 * 45)
  expect_equal(sum(a$by_member$transport_collective),
               sum(.trips_ref(q, 5) * 2 * 3 * 20) + bulk)
  expect_output(print(a), "Total potential saving")
})

test_that("aggregation_savings validates inputs", {
  expect_error(aggregation_savings(1:3, 10, grade_shares = c(0.5, 0.4),
                                   grade_prices = c(1, 2),
                                   ungraded_price = 1))
  expect_error(aggregation_savings(1:3, 10, grade_shares = 1))
  expect_error(aggregation_savings(1:3, 10,
                                   discount_tiers = data.frame(a = 1)))
})
