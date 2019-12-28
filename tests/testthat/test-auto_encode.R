
set.seed(1071)
design_mat_1 <- cbind( data.frame(matrix(rnorm(5*100),ncol = 5)),
                     sample(sample(letters, 10), 100, replace = TRUE))
colnames(design_mat_1)[6] <- "few_letters"

design_mat_2 <- cbind( design_mat_1,
                       sample(sample(letters, 20), 100, replace = TRUE))
colnames(design_mat_2)[7] <- "many_letters"

design_mat_3 <- cbind( design_mat_2,
                       sample(sample(1:10, 5), 100, replace = TRUE))
colnames(design_mat_3)[8] <- "some_numbers"

design_mat_4 <- cbind( design_mat_3,
                       sample(sample(1:50, 35), 100, replace = TRUE ))
colnames(design_mat_4)[9] <- "many_numbers"

design_mat_4 <- cbind( design_mat_4,
                       sample(1:2, 100, replace = TRUE ))
colnames(design_mat_4)[10] <- "binary"

test_that("Automatic encoding works", {
  
  
  expect_equal(2 * 2, 4)
})
