context("Test stratified_risk")

# test rrmh is expected value
testthat::test_that("rrmh returns expected value", {
  data("tolbutamide")
  expect_equal(stratified_risk(tolbutamide, exposure = tolbutamide, outcome = dead,
    stratifier = age)$rrmh[[1]], 1.325, tol = 0.001)

})

testthat::test_that("lci return expected value", {
  data("tolbutamide")
  expect_equal(stratified_risk(tolbutamide, exposure = tolbutamide, outcome = dead,
                               stratifier = age)$lci[[1]], 0.797, tol = 0.001)

  expect_equal(stratified_risk(tolbutamide, exposure = tolbutamide, outcome = dead,
                               stratifier = age, ci_level = 90)$lci[[1]], 0.865,
               tol = 0.001)

  expect_equal(stratified_risk(tolbutamide, exposure = tolbutamide, outcome = dead,
                               stratifier = age, ci_level = 99)$lci[[1]], 0.68,
               tol = 0.001)

})

testthat::test_that("uci return expected value", {
  data("tolbutamide")
  expect_equal(stratified_risk(tolbutamide, exposure = tolbutamide, outcome = dead,
                               stratifier = age)$uci[[1]], 2.20, tol = 0.001)

  expect_equal(stratified_risk(tolbutamide, exposure = tolbutamide, outcome = dead,
                               stratifier = age, ci_level = 90)$uci[[1]], 2.03,
               tol = 0.001)

  expect_equal(stratified_risk(tolbutamide, exposure = tolbutamide, outcome = dead,
                               stratifier = age, ci_level = 99)$uci[[1]], 2.582,
               tol = 0.001)

})
