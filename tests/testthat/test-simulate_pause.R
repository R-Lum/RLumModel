context("simulate_pause")

parms <- .set_pars("Bailey2001")
n <- parms$n$n
test_simulate_pause <- .simulate_pause(temp = 20, duration = 1, n = n, parms = parms)

test_that("check output",{
  expect_equal(class(test_simulate_pause)[1], "RLum.Results")
  
  expect_equal(length(test_simulate_pause$n), length(parms$N) + 2)
  
  expect_equal(test_simulate_pause$temp, 20)
  
})

