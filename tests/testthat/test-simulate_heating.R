context("simulate_heating")

parms <- .set_pars("Bailey2001")
n <- parms$n$n
test_simulate_heating <- .simulate_heating(
  temp_begin = 20,
  temp_end = 50,
  heating_rate = 5,
  n = n,
  parms = parms)

test_that("check output",{
  expect_equal(class(test_simulate_heating)[1], "RLum.Results")
  
  expect_equal(length(test_simulate_heating$n), length(parms$N) + 2)
  
  expect_equal(test_simulate_heating$temp, 50)
  
})

