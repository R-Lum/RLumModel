context("translate_sequence")

sequence <- list(OSL = c(125,10,90))
model <- "Bailey2001"
parms <- .set_pars(model)
n <- parms$n


test_translate_sequence <- .translate_sequence(
  sequence = sequence,
  n = n, 
  parms = parms, 
  model = model,
  txtProgressBar = FALSE, 
  verbose = FALSE)



test_that("check output",{
  expect_equal(class(test_translate_sequence)[1], "RLum.Analysis")
  
  expect_equal(class(test_translate_sequence@records), "list")
  
  expect_equal(class(test_translate_sequence@records[[1]])[1], "RLum.Data.Curve")
  
  expect_equal(length(test_translate_sequence), length(parms$N) + 2 + 1)
  
  expect_true("OSL" %in% names(test_translate_sequence))
  
  expect_equal(test_translate_sequence@originator, ".translate_sequence")
  
  expect_equal(test_translate_sequence@protocol, "Bailey2001")
  
  expect_equal(test_translate_sequence@info$sequence$OSL["temp"], c(temp = 125))
  
  expect_equal(test_translate_sequence@info$sequence$OSL["duration"], c(duration = 10))
  
  expect_equal(test_translate_sequence@info$sequence$OSL["optical_power"], c(optical_power = 90))

})
