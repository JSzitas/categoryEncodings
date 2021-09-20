test_that("Data validation works", {
  df <- data.frame( x_1 = rep(1, 100),
                    x_na = rep(NA_real_, 100),
                    fact_1 = sample( letters[1:3], size = 100, replace = TRUE ),
                    fact_2 = rep("a", 100)
  )
  
  expect_error( validate_data( df, fact = "fact_1" ), 
                "Variable x_1 only has one unique value - therefore it is invalid for encoding.")
  
  df <- data.frame( x_1 = rnorm(100),
                    x_na = rep(NA_real_, 100),
                    fact_1 = sample( letters[1:3], size = 100, replace = TRUE ),
                    fact_2 = rep("a", 100)
  )
  
  expect_error( validate_data( df, fact = "fact_1" ), 
                "Variable x_na only contains missing values - please drop it manually.")
  
  
  df <- data.frame( x_1 = rnorm(100),
                    fact_1 = sample( letters[1:3], size = 100, replace = TRUE ),
                    fact_2 = rep("a", 100)
  )
  
  expect_error( validate_data( df, fact = "fact_1" ))
  expect_error( validate_data( df, fact = c( "fact_1", "fact_2") ),
                "Factor fact_2 only has one unique value - therefore it is invalid for encoding.")
  # fact_3 does not exist in the data - hence we should fail
  expect_error( validate_data( df, fact = c( "fact_1", "fact_2", "fact_3") ),
                "Factor fact_3 not found in table.")
  
  
  expect_error( validate_data( df, fact = c( "fact_2") ), 
                "Factor fact_2 only has one unique value - therefore it is invalid for encoding.")

})
