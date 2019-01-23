context("Test rate")

cancer_xray <- data.frame(cases = c(41, 15), pyar = c(28010, 19017),
                          radiation = c(1, 0))

test_that("Test rate_ratio returns exected estimate", {
  expect_equal(rate(data = cancer_xray, outcome = cases, denominator = pyar,
                    exposure = radiation, per = 100000)$rate_ratio[[2]],
               1.855, tol = 0.001)
})

test_that("Test rate_diff returns exected estimate", {
  expect_equal(rate(data = cancer_xray, outcome = cases, denominator = pyar,
                    exposure = radiation, per = 100000)$rate_diff[[2]],
               67.499, tol = 0.001)
})

test_that("Test rate_ratio returns exected lci", {
  expect_equal(rate(data = cancer_xray, outcome = cases, denominator = pyar,
                    exposure = radiation, per = 100000)$rate_ratio_lci[[2]],
               1.03, tol = 0.01)
})

test_that("Test rate_ratio returns exected uci", {
  expect_equal(rate(data = cancer_xray, outcome = cases, denominator = pyar,
               exposure = radiation, per = 100000)$rate_ratio_uci[[2]],
  3.35, tol = 0.001)
})

test_that("Test rate_diff returns exected lci", {
  expect_equal(rate(data = cancer_xray, outcome = cases, denominator = pyar,
                    exposure = radiation, per = 100000)$rate_diff_lci[[2]],
               7.5, tol = 0.001)
})

test_that("Test rate_diff returns exected uci", {
  expect_equal(rate(data = cancer_xray, outcome = cases, denominator = pyar,
                    exposure = radiation, per = 100000)$rate_diff_uci[[2]],
               128, tol = 0.1)
})
