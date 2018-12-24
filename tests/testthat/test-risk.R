context("Test risk")

# dat <- data.frame(
#     exposure_var = c(rep(1, 8), rep(0, 5), rep(1, 98), rep(0, 115)),
#   outcome_var = c(rep(1, 8), rep(1, 5), rep(0, 98), rep(0, 115)),
#   stringsAsFactors = FALSE
# )

# p 248 table 14-4 for check
dat <- data.frame(
  exposure_var = c(rep(0, 7), rep(1, 12), rep(0, 9), rep(1, 2)),
  outcome_var = c(rep(1, 7), rep(1, 12), rep(0,9), rep(0, 2))
)

test_that("Test risk_ratio returns exected estimate", {
  expect_equal(risk(data = dat, exposure = exposure_var, outcome = outcome_var
                    )$risk_ratio[[2]], 1.96, tol = 0.01)
})

test_that("Test risk_ratio returns exected lci", {
  expect_equal(risk(data = dat, exposure = exposure_var, outcome = outcome_var
  )$rr_lci[[2]], 1.1, tol = 0.1)
})

test_that("Test risk_ratio returns exected uci", {
  expect_equal(risk(data = dat, exposure = exposure_var, outcome = outcome_var
  )$rr_uci[[2]], 3.6, tol = 0.1)
})

test_that("Test risk_diff returns exected estimate", {
  expect_equal(risk(data = dat, exposure = exposure_var, outcome = outcome_var
  )$risk_diff[[2]], 42, tol = 0.001)
})

test_that("Test risk_diff returns exected lci", {
  expect_equal(risk(data = dat, exposure = exposure_var, outcome = outcome_var
  )$rd_lci[[2]], 10, tol = 0.1)
})

test_that("Test risk_diff returns exected uci", {
  expect_equal(risk(data = dat, exposure = exposure_var, outcome = outcome_var
  )$rd_uci[[2]], 73, tol = 0.1)
})
