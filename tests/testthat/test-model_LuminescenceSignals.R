context("model_LuminescenceSignals")

test_that("Models are models available",{
  expect_error(model_LuminescenceSignals(sequence = list(OSL = c(20,1,100))),"argument \"model\" is missing")
  expect_error(model_LuminescenceSignals(model = "Bailey2001"),"argument \"sequence\" is missing")

})

test_that("Output is RLum.Analysis",{
  expect_is(model_LuminescenceSignals(model = "Bailey2001", sequence = list(OSL = c(20,1,100)), plot = FALSE, verbose = FALSE), "RLum.Analysis")

})

test_that("Doserate > 0",{
  expect_error(model_LuminescenceSignals(model = "Bailey2001", sequence = list(OSL = c(20,1,100)), plot = FALSE, verbose = FALSE,lab.DoseRate = -1), "lab.DoseRate has to be a positive number")

})
