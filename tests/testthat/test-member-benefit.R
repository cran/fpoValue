mb_args <- list(quantity = c(5, 8, 12, 20, 35, 60), outside_price = 1400,
                member_price = 1480, sale_price = 1650, outside_cost = 140,
                member_cost = 40, handling_cost = 60, fixed_cost = 8000,
                input_quantity = c(1, 1.6, 2.4, 4, 7, 12),
                input_market_price = 1350, input_member_price = 1300,
                input_bulk_price = 1250, dividend_share = 0.5)

test_that("FPO and member accounts reconcile", {
  mb <- do.call(member_benefit, mb_args)
  Q <- 140
  expect_equal(mb$fpo[["output_margin"]], Q * (1650 - 1480 - 60))
  expect_equal(mb$fpo[["input_margin"]], 28 * 50)
  expect_equal(mb$fpo[["net_surplus"]], 15400 + 1400 - 8000)
  expect_equal(sum(mb$members$dividend), mb$fpo[["dividends"]])
  expect_equal(mb$value[["value_created"]],
               sum(mb$members$total) + mb$fpo[["retained"]])
  expect_equal(mb$value[["value_created"]],
               sum(mb$members$total - mb$members$dividend) +
                 mb$fpo[["net_surplus"]])
  expect_output(print(mb), "Value created")
})

test_that("dividend rules allocate the pool correctly", {
  a <- mb_args
  a$dividend_rule <- "equal"
  mb <- do.call(member_benefit, a)
  expect_equal(length(unique(round(mb$members$dividend, 8))), 1)
  a$dividend_rule <- "equity"
  expect_error(do.call(member_benefit, a), "equity")
  a$equity <- c(1, 1, 1, 1, 1, 5)
  mb2 <- do.call(member_benefit, a)
  expect_equal(mb2$members$dividend[6], 5 * mb2$members$dividend[1])
})

test_that("no dividend is paid from a loss", {
  a <- mb_args
  a$fixed_cost <- 1e6
  mb <- do.call(member_benefit, a)
  expect_lt(mb$fpo[["net_surplus"]], 0)
  expect_equal(sum(mb$members$dividend), 0)
})

test_that("distribution metrics behave as expected", {
  a <- mb_args
  a$group <- c("marginal", "marginal", "small", "small", "medium", "large")
  mb <- do.call(member_benefit, a)
  bg <- mb$distribution$by_group
  expect_equal(sum(bg$benefit_share), 1)
  expect_equal(sum(bg$members), 6)
  # higher outside cost for small suppliers makes benefits progressive
  a$outside_cost <- c(300, 250, 180, 140, 100, 80)
  mb2 <- do.call(member_benefit, a)
  m2 <- mb2$distribution$metrics
  expect_lt(m2[["concentration_index"]],
            mb$distribution$metrics[["concentration_index"]])
  expect_gt(m2[["bottom50_benefit_share"]], m2[["bottom50_quantity_share"]])
  # negative benefits disable Gini and are counted
  a$member_price <- 1200
  mb3 <- do.call(member_benefit, a)
  expect_true(is.na(mb3$distribution$metrics[["gini"]]) ||
                mb3$distribution$metrics[["share_members_worse_off"]] == 0)
  expect_error(member_benefit(1:3, 1, 1, 1, group = "a"))
})
