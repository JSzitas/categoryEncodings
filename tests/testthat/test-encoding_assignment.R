
set.seed(1071)
df <- data.frame( y = rnorm(100), 
                  x_1 = rnorm(100), 
                  x_2 = rpois(100, 0.5), 
                  fact_1 = sample( letters[1:3], size = 100, replace = TRUE ),
                  fact_2 = sample( letters[1:5], size = 100, replace = TRUE )
)
dt <- data.table::as.data.table(df)

test_that("Assigning encodings works", {
  expect_equal( assign_encodings( dt, fact = c("fact_1","fact_2") ), 
                list( fact_1 = encode_spca, 
                      fact_2 = encode_mean )
                )
  
  expect_equal( assign_encodings( dt,
                                  fact = c("fact_1","fact_2"),
                                  methods = c("dummy","mean")
                                  ), 
                list( fact_1 = encode_dummy, 
                      fact_2 = encode_mean )
  )
})

test_that( "Providing a custom encoding scheme works", {
  custom_assigner <- function( X, fact ){ return(rep("dummy", length(fact))) }
  expect_equal( assign_encodings( dt,
                                  fact = c("fact_1","fact_2"),
                                  custom_assignment_method = custom_assigner
  ), 
  list( fact_1 = encode_dummy, 
        fact_2 = encode_dummy )
  )
})
