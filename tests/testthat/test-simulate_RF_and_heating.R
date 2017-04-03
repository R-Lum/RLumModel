context("simulate_RF_and_heating")


parms <- .set_pars("Bailey2001")
n <- parms$n$n
test_simulate_RF_and_heating <- .simulate_RF_and_heating(
  temp_begin = 20, 
  temp_end = 500,
  heating_rate = 1,
  dose_rate = 0.06, 
  n = n, 
  parms = parms)


test_that("check output",{
  expect_equal(class(test_simulate_RF_and_heating)[1], "RLum.Results")
  
  expect_equal(length(test_simulate_RF_and_heating$n), length(parms$N) + 2)
  
  expect_equal(test_simulate_RF_and_heating$temp, 500)
  
  ##check concentrations
  expect_equal(length(test_simulate_RF_and_heating$concentrations), length(parms$N) + 2)
  
  expect_equal(class(test_simulate_RF_and_heating$concentrations), "list")
  
  expect_equal(class(test_simulate_RF_and_heating$concentrations[[1]])[1], "RLum.Data.Curve")
  
})

