set.seed(1071)
design_mat <- cbind( data.frame(matrix(rnorm(5*100),ncol = 5)),
                     sample(sample(letters, 10), 100, replace = TRUE))
colnames(design_mat)[6] <- "factor_var"

test_that("Repeated effect encoding works", {
  new_mat <- encode_repeated_effect(design_mat, "factor_var")
  
  expect_equal(ncol(new_mat), 14)
  expect_equal(nrow(new_mat), 100)
  expect_equal(sum(apply(new_mat,2, is.numeric)), 14)
  
  new_mat <- encode_repeated_effect(design_mat, "factor_var", keep_factor = TRUE)
  
  expect_equal(ncol(new_mat), 15)
  expect_equal(nrow(new_mat), 100)
  expect_equal(sum(unlist(lapply(new_mat, is.numeric))), 14)
})
