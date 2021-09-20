test_that("Utility functions work", {
  
  expect_equal( len_unique( c(1) ), 1 )
  expect_equal( len_unique( c(NA) ), 1 )
  expect_equal( len_unique( c(1,1) ), 1 )
  expect_equal( len_unique( c(1:10) ), 10 )

  set.seed(1071)
  random_sample <- sample(c("a","b","c","d"), size = 100, replace = TRUE)
  expect_true( is_likely_factor(random_sample)  )
  
  df <- data.frame( a = random_sample, b = rnorm(100))
  expect_equal( is_likely_factor_df(df), c( a = TRUE, b = FALSE ))
  
  dt <- data.table::as.data.table( df )
  expect_type( handle_y( X = dt, Y = NULL ), "list")
  expect_type( handle_y( X= dt, Y = "a" ), "list" )
  expect_type( handle_y( X= dt, Y = 1 ), "list" )

  df <- data.frame( y = rnorm(100), 
                    x_1 = rnorm(100), 
                    x_2 = rpois(100, 0.5), 
                    x_3 = rep(1, 100),
                    x_na = rep(NA_real_, 100),
                    fact_1 = sample( letters[1:3], size = 100, replace = TRUE ),
                    fact_2 = rep("a", 100)
  )
  
  
  expect_equal( handle_factors( df, c("fact_1", "fact_2")),
                c("fact_1", "fact_2") 
                )
  expect_equal( handle_factors( df, 6:7),
                c("fact_1", "fact_2") 
  )
  expect_equal( suppressWarnings(handle_factors( df, NULL)), 
                c("x_2", "x_3", "x_na", "fact_1", "fact_2"))
  expect_warning(handle_factors( df, NULL))
  
  df <- data.frame( fact_1 = sample( letters[1:3], size = 100, replace = TRUE ) )
  expect_error(handle_factors( df, NULL))

  old_ <- c("a","b","c")
  new_ <- c("a","b","d")
  expect_error( check_colnames( old_, new_ ), "New data is missing columns: \nc" ) 
  
  old_ <- c("a","b","c")
  new_ <- c("a","b","c")
  expect_invisible( check_colnames( old_, new_ ) )
  
  expect_s3_class( as_dt_tibble( tibble::tibble(a = 10)), "data.table" )
  
  expect_s3_class( to_data_table( data.frame(a = 10)), "data.table" )
  expect_s3_class( to_data_table( tibble::tibble(a = 10)), "data.table" )
})
