test_that("all requested data files are succesfully read in", {
  featurefiles <- RFQAmodelr::collect_files(directory = system.file("extdata", package = "RFQAmodelr"))
  expect_equal(min(lengths(featurefiles)) > 0 , TRUE)
})

test_that("Test 5EXP_A input data matches",{
  input <- RFQAmodelr::collect_input(directory = system.file("extdata", package = "RFQAmodelr"))
  load(system.file("data","test_input.rda", package = "RFQAmodelr"))
  expect_equal(input, test_input)
})
