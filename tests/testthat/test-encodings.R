
set.seed(1071)
design_mat <- cbind( data.frame(matrix(rnorm(5*100),ncol = 5)),
                     sample(sample(letters, 10), 100, replace = TRUE))
colnames(design_mat)[6] <- "factor_var"


test_that("Mean encoding works", {
  new_mat <- encode_mean(design_mat, "factor_var")
  
  expect_equal(ncol(new_mat), 10)
  expect_equal(nrow(new_mat), 100)
  expect_equal(sum(apply(new_mat,2, is.numeric)), 10)
  
  new_mat <- encode_mean(design_mat, "factor_var", keep_factor = TRUE)
  
  expect_equal(ncol(new_mat), 11)
  expect_equal(nrow(new_mat), 100)
  expect_equal(sum(unlist(lapply(new_mat, is.numeric))), 10)
})

test_that("Low rank encoding works", {
  new_mat <- encode_lowrank(design_mat, "factor_var")
  
  expect_equal(ncol(new_mat), 10)
  expect_equal(nrow(new_mat), 100)
  expect_equal(sum(apply(new_mat,2, is.numeric)), 10)
  
  new_mat <- encode_lowrank(design_mat, "factor_var", keep_factor = TRUE)
  
  expect_equal(ncol(new_mat), 11)
  expect_equal(nrow(new_mat), 100)
  expect_equal(sum(unlist(lapply(new_mat, is.numeric))), 10)
})

test_that("Median encoding works", {
  new_mat <- encode_median(design_mat, "factor_var")
  
  expect_equal(ncol(new_mat), 10)
  expect_equal(nrow(new_mat), 100)
  expect_equal(sum(apply(new_mat,2, is.numeric)), 10)
  
  new_mat <- encode_median(design_mat, "factor_var", keep_factor = TRUE)
  
  expect_equal(ncol(new_mat), 11)
  expect_equal(nrow(new_mat), 100)
  expect_equal(sum(unlist(lapply(new_mat, is.numeric))), 10)
})

test_that("Mnl encoding works", {
  new_mat <- encode_mnl(design_mat, "factor_var")
  
  expect_equal(ncol(new_mat), 11)
  expect_equal(nrow(new_mat), 100)
  expect_equal(sum(apply(new_mat,2, is.numeric)), 11)
  
  new_mat <- encode_mnl(design_mat, "factor_var", keep_factor = TRUE)
  
  expect_equal(ncol(new_mat), 12)
  expect_equal(nrow(new_mat), 100)
  expect_equal(sum(unlist(lapply(new_mat, is.numeric))), 11)
})


test_that("SPCA encoding works", {
  new_mat <- encode_SPCA(design_mat, "factor_var")
  
  expect_equal(ncol(new_mat), 10)
  expect_equal(nrow(new_mat), 100)
  expect_equal(sum(apply(new_mat,2, is.numeric)), 10)
  
  new_mat <- encode_SPCA(design_mat, "factor_var", keep_factor = TRUE)
  
  expect_equal(ncol(new_mat), 11)
  expect_equal(nrow(new_mat), 100)
  expect_equal(sum(unlist(lapply(new_mat, is.numeric))), 10)
})

test_that("Deviation encoding works", {
  new_mat <- encode_deviation(design_mat, "factor_var")
  
  expect_equal(ncol(new_mat), 14)
  expect_equal(nrow(new_mat), 100)
  expect_equal(sum(apply(new_mat,2, is.numeric)), 14)
  
  new_mat <- encode_deviation(design_mat, "factor_var", keep_factor = TRUE)
  
  expect_equal(ncol(new_mat), 15)
  expect_equal(nrow(new_mat), 100)
  expect_equal(sum(unlist(lapply(new_mat, is.numeric))), 14)
})




