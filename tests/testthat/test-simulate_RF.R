context("simulate_RF")


parms <- .set_pars("Bailey2001")
n <- parms$n$n
test_simulate_RF <- .simulate_RF(temp = 20, dose = 1, dose_rate = 1, n = n, parms = parms)


test_that("check output",{
  expect_equal(class(test_simulate_RF)[1], "RLum.Results")
  
  expect_equal(length(test_simulate_RF$n), length(parms$N) + 2)
  
  expect_equal(test_simulate_RF$temp, 20)
  
  ##check concentrations
  expect_equal(length(test_simulate_RF$concentrations), length(parms$N) + 2)
  
  expect_equal(class(test_simulate_RF$concentrations), "list")
  
  expect_equal(class(test_simulate_RF$concentrations[[1]])[1], "RLum.Data.Curve")
  
})

