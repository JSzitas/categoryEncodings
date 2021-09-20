test_that("Method object literal works", {
  expect_equal( get_encoding_method( "mean" ), encode_mean)
})

test_that("Valdating a method works", {
  expect_invisible( validate_encoding_methods(c("mean","spca")) )
  expect_error( validate_encoding_methods(c("means")) )
  expect_error( validate_encoding_methods(c("SPCA")) )
})
