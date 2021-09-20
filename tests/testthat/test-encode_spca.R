set.seed(1071)
design_mat <- cbind( data.frame(matrix(rnorm(5*100),ncol = 5)),
                     sample(sample(letters, 10), 100, replace = TRUE))
colnames(design_mat)[6] <- "factor_var"

test_that("SPCA encoding works", {
  new_mat <- encode_spca(design_mat, "factor_var")
  
  expect_equal(ncol(new_mat), 10)
  expect_equal(nrow(new_mat), 100)
  expect_equal(sum(apply(new_mat,2, is.numeric)), 10)
  
  new_mat <- encode_spca(design_mat, "factor_var", keep_factor = TRUE)
  
  expect_equal(ncol(new_mat), 11)
  expect_equal(nrow(new_mat), 100)
  expect_equal(sum(unlist(lapply(new_mat, is.numeric))), 10)
})

test_that( "Passing arguments to the underlying spca function works", {
  new_mat <- encode_spca(design_mat, "factor_var", keep_factor = FALSE, k = 3)
  
  expect_equal(ncol(new_mat), 8)
  expect_equal(nrow(new_mat), 100)
  expect_equal(sum(apply(new_mat,2, is.numeric)), 8)
  
  new_mat <- encode_spca(design_mat, "factor_var", keep_factor = FALSE, alpha = 0.2)
  
  expect_equal(ncol(new_mat), 8)
  expect_equal(nrow(new_mat), 100)
  expect_equal(sum(unlist(lapply(new_mat, is.numeric))), 8)
})