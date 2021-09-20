
set.seed(1071)
design_mat <- cbind( data.frame(matrix(rnorm(5*100),ncol = 5)),
                     sample(sample(letters, 10), 100, replace = TRUE))
colnames(design_mat)[6] <- "factor_var"

test_that("Dummy encoding works",{
  
  result <- encode_dummy(design_mat, fact = 6)
  
  expected_6th_col <-
    list(factor_var_b_dummy = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 
                                1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                0, 0, 0, 0, 0))
  expect_equal( c(result[,6]), expected_6th_col )
  expect_equal(ncol(result), 14)
})
