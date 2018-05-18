 context("fit_RLumModel2data")
 
 sequence <- list(
   IRR = c(20, 1, 1),
   TL = c(20, 200, 5))
   
  model <- "Bailey2001"

  test_that("check output",{
    
    func_FME <- fit_RLumModel2data(
      sequence = sequence, 
      model = model, 
      seq.step2fit = 2)
    
    expect_true(is.function(func_FME))
    
  #### check own_parameters function
  
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
  
  func_FME <- fit_RLumModel2data(
    sequence = sequence, 
    model = "customized",
    own_parameters = own_parameters, 
    seq.step2fit = 2)
  })
  
  
  test_that("test controlled crash conditions",{
    
    expect_error(fit_RLumModel2data(model = "Bailey2000"), 
                 regexp = "[extract_parameters2FME()] Model not supported. Supported models are: Bailey2001, Bailey2004, Pagonis2008, Pagonis2007, Bailey2002, Friedrich2017, Friedrich2018",
                 fixed = TRUE)
    

  })