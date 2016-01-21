context("plot_concentrations")

test_that("input is RLum.Analysis",{
  expect_error(plot_concentrations(object = 2,record.id = 1),"not of type 'RLum.Analysis'")

})
