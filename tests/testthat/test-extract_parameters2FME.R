context("extract_parameters")

own_parameters <- list(
  N = c(1e13,0),
  E = c(1.3, 0),
  s = c(1e12, 0),
  A = c(1e-8, 0),
  B = c(0, 1e-8),
  Th = c(0, 0),
  E_th = c(0, 0),
  k_B = 8.617e-5,
  K = 0,
  model = "customized"
  )
  
parms_own_parameters <- extract_parameters2FME(parms = own_parameters)
  

parms <- extract_parameters2FME(model = "Bailey2001")

test_that("check length of output",{
  
  expect_length(parms, 55)

})

test_that("test controlled crash conditions",{
  
  expect_error(extract_parameters2FME(model = "Bailey2000"), 
               regexp = "[extract_parameters2FME()] Model not supported. Supported models are: Bailey2001, Bailey2004, Pagonis2008, Pagonis2007, Bailey2002, Friedrich2017, Friedrich2018",
               fixed = TRUE)
  
  expect_error(extract_parameters2FME(), 
               regexp = "[extract_parameters2FME()] Either 'model' or 'parms' has to be a function argument.",
               fixed = TRUE)
  
  expect_error(extract_parameters2FME(parms = c(1)), 
               regexp = "[extract_parameters2FME()] Function argument 'parms' has to be of class list.",
               fixed = TRUE)
  
})
