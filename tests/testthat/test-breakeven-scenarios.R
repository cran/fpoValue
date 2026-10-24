test_that("break_even_volume is correct and consistent", {
  be <- break_even_volume(fixed_cost = 6e5, sale_price = 1650,
                          member_price = c(1450, 1500), handling_cost = 60,
                          input_margin = 1e5, avg_member_volume = 15,
                          current_volume = 6000)
  expect_equal(be$break_even_volume, 5e5 / c(140, 90))
  expect_equal(be$members_needed, ceiling(5e5 / c(140, 90) / 15))
  # paying max_member_price at current volume exactly breaks even
  pm <- be$max_member_price[1]
  expect_equal(6000 * (1650 - pm - 60) + 1e5 - 6e5, 0)
  neg <- break_even_volume(1000, 100, 120)
  expect_equal(neg$break_even_volume, Inf)
  expect_equal(break_even_volume(100, 10, 5, input_margin = 200)$
                 break_even_volume, 0)
  expect_error(break_even_volume(1:2, 1:3, 1))
})

test_that("scenario comparison matches member_benefit", {
  q <- c(5, 8, 12, 20, 35, 60)
  sc <- data.frame(scenario = c("trader", "FPO"),
                   member_price = c(1300, 1480), member_cost = c(10, 40),
                   sale_price = c(NA, 1650), handling_cost = c(0, 60),
                   fixed_cost = c(0, 8000), dividend_share = c(0, 0.5))
  res <- collective_marketing_scenarios(q, sc, group = rep(c("s", "l"),
                                                           each = 3))
  mb <- member_benefit(q, outside_price = 1300, member_price = 1480,
                       sale_price = 1650, outside_cost = 10,
                       member_cost = 40, handling_cost = 60,
                       fixed_cost = 8000, dividend_share = 0.5)
  s <- res$summary
  expect_equal(s$member_gain[2], mb$value[["member_benefit"]])
  expect_equal(s$retained[2], mb$fpo[["retained"]])
  expect_equal(s$value_created[2], mb$value[["value_created"]])
  expect_equal(dim(res$group_gain), c(2L, 2L))
  expect_output(print(res), "baseline: trader")
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off())
  expect_invisible(plot(res))
})

test_that("scenario inputs are validated", {
  sc <- data.frame(scenario = c("a", "a"), member_price = 1,
                   member_cost = 0, sale_price = NA)
  expect_error(collective_marketing_scenarios(1:2, sc), "unique")
  expect_error(collective_marketing_scenarios(1:2, sc[1, ]))
  sc$scenario <- c("a", "b")
  expect_error(collective_marketing_scenarios(1:2, sc, baseline = "z"))
  expect_error(collective_marketing_scenarios(1:2, sc[, -2]), "lacks")
})
