context("Test risk")

dat <- data.frame(
    exposure_var = c(rep(1, 8), rep(0, 5), rep(1, 98), rep(0, 115)),
  outcome_var = c(rep(1, 8), rep(1, 5), rep(0, 98), rep(0, 115)),
  stringsAsFactors = FALSE
)

test_that("Test risk_ratio returns exected estimate", {
  expect_equal(risk(data = dat, exposure = exposure_var, outcome = outcome_var
                    )$risk_ratio[[2]], 1.811, tol = 0.001)
})

test_that("Test risk_ratio returns exected lci", {
  expect_equal(risk(data = dat, exposure = exposure_var, outcome = outcome_var
  )$rr_lci[[2]], 0.728, tol = 0.001)
})

test_that("Test risk_ratio returns exected uci", {
  expect_equal(risk(data = dat, exposure = exposure_var, outcome = outcome_var
  )$rr_uci[[2]], 4.51, tol = 0.001)
})

test_that("Test risk_diff returns exected estimate", {
  expect_equal(risk(data = dat, exposure = exposure_var, outcome = outcome_var
  )$risk_diff[[2]], 3.381, tol = 0.001)
})

test_that("Test risk_diff returns exected lci", {
  expect_equal(risk(data = dat, exposure = exposure_var, outcome = outcome_var
  )$rd_lci[[2]], 3.21, tol = 0.001)
})

test_that("Test risk_diff returns exected uci", {
  expect_equal(risk(data = dat, exposure = exposure_var, outcome = outcome_var
  )$rd_uci[[2]], 3.56, tol = 0.001)
})
