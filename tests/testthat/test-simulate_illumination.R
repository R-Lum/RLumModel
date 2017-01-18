context("simulate_illumination")


parms <- .set_pars("Bailey2001")
n <- parms$n$n
test_simulate_illumination <- .simulate_illumination(
  temp = 125, 
  duration = 100, 
  optical_power = 90, 
  n = n, 
  parms = parms)


test_that("check output",{
  expect_equal(class(test_simulate_illumination)[1], "RLum.Results")
  
  expect_equal(length(test_simulate_illumination$n), length(parms$N) + 2)
  
  expect_equal(test_simulate_illumination$temp, 125)

})

