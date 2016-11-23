context("simulate_CW_OSL")


parms <- .set_pars("Bailey2001")
n <- parms$n$n
test_simulate_CW_OSL <- .simulate_CW_OSL(temp = 125, duration = 100, n = n, parms = parms)


test_that("check output",{
  expect_equal(class(test_simulate_CW_OSL)[1], "RLum.Results")
  
  expect_equal(length(test_simulate_CW_OSL$n), length(parms$N) + 2)
  
  expect_equal(test_simulate_CW_OSL$temp, 125)
  
  ##check concentrations
  expect_equal(length(test_simulate_CW_OSL$concentrations), length(parms$N) + 2)
  
  expect_equal(class(test_simulate_CW_OSL$concentrations), "list")
  
  expect_equal(class(test_simulate_CW_OSL$concentrations[[1]])[1], "RLum.Data.Curve")
  
})

