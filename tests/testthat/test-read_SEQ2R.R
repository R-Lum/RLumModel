context("read_SEQ2R")

path <- system.file("extdata", "example_SAR_cycle.SEQ", package = "RLumModel")

test_that("file is *.SEQ file",{
  expect_error(read_SEQ2R(file = 2), "class of file has to be a character")
  expect_error(read_SEQ2R(), "argument \"file\" is missing")
})

test_that("output is list",{
  expect_equal(class(read_SEQ2R(file = path, txtProgressBar = FALSE)), "list")
})

test_that("lab.dose_rate > 0",{
  expect_error(read_SEQ2R(file = path, txtProgressBar = FALSE, lab.dose_rate =  -1), "Argument 'lab.dose_rate' has to be positiv")
})