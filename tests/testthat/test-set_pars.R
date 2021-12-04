test_that("chrash function",{
  skip_on_cran()
  local_edition(3)

  expect_error(.set_pars("error"),
               "\\[\\.set_Pars\\(\\)\\] Model not supported. Supported models are: Bailey2001, Bailey2004, Pagonis2008, Pagonis2007, Bailey2002, Friedrich2017, Friedrich2018, customized, customised")


})

test_that("check length of output",{
  skip_on_cran()
  local_edition(3)

  expect_equal(length(.set_pars("Bailey2001")), 12)
  expect_equal(length(.set_pars("Bailey2002")), 12)
  expect_equal(length(.set_pars("Bailey2004")), 12)
  expect_equal(length(.set_pars("Pagonis2007")), 12)
  expect_equal(length(.set_pars("Pagonis2008")), 12)
  expect_equal(length(.set_pars("Friedrich2017")), 12)
  expect_equal(length(.set_pars("customized")), 5)
  expect_equal(length(.set_pars("customised")), 5)

})

test_that("check class of output",{
  skip_on_cran()
  local_edition(3)

  expect_equal(class(.set_pars("Bailey2001")), "list")
  expect_equal(class(.set_pars("Bailey2002")), "list")
  expect_equal(class(.set_pars("Bailey2004")), "list")
  expect_equal(class(.set_pars("Pagonis2007")), "list")
  expect_equal(class(.set_pars("Pagonis2008")), "list")
  expect_equal(class(.set_pars("Friedrich2017")), "list")
  expect_equal(class(.set_pars("customized")), "list")
  expect_equal(class(.set_pars("customised")), "list")

})
