context("simulate_irradiation")


parms <- .set_pars("Bailey2001")
n <- parms$n$n
test_simulate_irradiation <- .simulate_irradiation(temp = 20, dose = 1, dose_rate = 1, n = n, parms = parms)


test_that("check output",{
  expect_equal(class(test_simulate_irradiation)[1], "RLum.Results")
  
  expect_equal(length(test_simulate_irradiation$n), length(parms$N) + 2)
  
  expect_equal(test_simulate_irradiation$temp, 20)

})

