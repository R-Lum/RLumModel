context("set parameters")

test_that("check inputs set_Pars",{
  expect_output(.set_Pars("Bailey2001"), "data: 12")
  expect_output(.set_Pars("Bailey2002"), "data: 12")
  expect_output(.set_Pars("Bailey2004"), "data: 12")
  expect_output(.set_Pars("Pagonis2007"), "data: 12")
  expect_output(.set_Pars("Pagonis2008"), "data: 12")
})
