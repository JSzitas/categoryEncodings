
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

test_that("Automatic encoding works with specified methods", {
  
    result <- suppressWarnings(
       encode_categories(design_mat_1, method = "mean")
       )

   expect_equal(ncol(result), 10)
   expect_equal(result, encode_mean(design_mat_1, fact = 6))
   
   result <- suppressWarnings(
      encode_categories(design_mat_1, method = "median"))
   
   expect_equal(ncol(result), 10)
   expect_equal(result, encode_median(design_mat_1, fact = 6))
   
   result <- suppressWarnings(
      encode_categories(design_mat_1, method = "deviation"))
   
   expect_equal(ncol(result), 14)
   expect_equal(result, encode_deviation(design_mat_1, fact = 6))
   
   result <- suppressWarnings(
      encode_categories(design_mat_1, method = "dummy"))
   
   expect_equal(ncol(result), 14)
   expect_equal(result, encode_dummy(design_mat_1, fact = 6))
   
   result <- suppressWarnings(
      encode_categories(design_mat_1, method = "lowrank"))
   
   expect_equal(ncol(result), 10)
   expect_equal(result, encode_lowrank(design_mat_1, fact = 6))
   
   result <- suppressWarnings(
      encode_categories(design_mat_1, method = "SPCA"))
   
   expect_equal(ncol(result), 10)
   expect_equal(result, encode_SPCA(design_mat_1, fact = 6))
   
   result <- suppressWarnings(
      encode_categories(design_mat_1, method = "mnl"))
   
   expect_equal(ncol(result), 11)
   expect_equal(result, encode_mnl(design_mat_1, fact = 6))
})

test_that("Automatic encoding works with factor specification", {
   
   result <- encode_categories(design_mat_1, fact = 6)
   
   expect_equal(ncol(result), 10)
   expect_equal(result, encode_SPCA(design_mat_1, fact = 6))
   
   result <- encode_categories(design_mat_1, fact = 6)
   
   expect_equal(ncol(result), 10)
   expect_equal(result, encode_SPCA(design_mat_1, fact = 6))
   
})

test_that("Automatic encoding works with factor and method specification", {
   
   result <- encode_categories(design_mat_1, fact = 6, method = "mean")
   
   expect_equal(ncol(result), 10)
   expect_equal(result, encode_mean(design_mat_1, fact = 6))
   
   result <- encode_categories(design_mat_2, fact = 6:7, method = c("mean",
                                                                     "median"))
   
   expect_equal(ncol(result), 15)
})




test_that("Automatic encoding works without method specification", {
   
   result <- suppressWarnings(
      encode_categories(design_mat_1))
   
   expect_equal(ncol(result), 10)
   expect_equal(result, encode_SPCA(design_mat_1, fact = "few_letters"))
   
})

test_that("Warnings/Errors work", {
   
   expect_error( encode_categories(design_mat_1, fact = "few_letters",
                                    method = c("means","median")),
                 "Failed to match the supplied method(s).",
                 fixed = TRUE)
   
   too_few_methods <- paste("The number of supplied methods(", 2,
         ") is not equal to the number of factors(", 5,").", 
         "Please specify the correct number of methods, and/or factor variables.",
         sep = "")
   too_many_methods <- paste("More methods(", 2,") than factors
  (", 1,") detected, using only the first few, 
           until there is enough methods for the number of factor variables, 
           and dropping the rest.", sep = "")
   
   expect_error( suppressWarnings(
      encode_categories( design_mat_4, method = c("mean","median"))),
                 too_few_methods,
                 fixed = TRUE )
   
   expect_warning( encode_categories(design_mat_1, fact = "few_letters",
                                   method = c("mean","median")),
                   too_many_methods,
                   fixed = TRUE )
})

test_that("Specifying Y works", {
   
   result <- suppressWarnings(
      encode_categories(X = design_mat_1, Y = "X1"))
   
   expect_equal(ncol(result), 9)
   
   result <- suppressWarnings(
      encode_categories(X = design_mat_1, Y = 3))
   
   expect_equal(ncol(result), 9)
   
})

test_that("Factor warnings work", {
   
   expect_warning( encode_categories(X = design_mat_4))
   
   
})

test_that("Inferring that everything is a factor raises error", {
  expect_error(encode_categories(design_mat_4[,6:10]))
})



