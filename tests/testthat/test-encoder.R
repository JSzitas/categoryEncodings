
set.seed(1071)
df <- data.frame(matrix(rnorm(5 * 100), ncol = 5),
  few_letters = sample(sample(letters, 10), 100, replace = TRUE),
  many_letters = sample(sample(letters, 20), 100, replace = TRUE),
  some_numbers = sample(sample(1:10, 5), 100, replace = TRUE),
  many_numbers = sample(sample(1:50, 35), 100, replace = TRUE),
  binary = sample(1:2, 100, replace = TRUE)
)

result_head <- structure(list(
  X1 = 0.775600110149059, X2 = -0.516696964133137,
  X3 = -2.40900548382913, X4 = 0.802633895107748, X5 = 0.848441805041443,
  few_letters_X1_mean = 0.270057297885454, few_letters_X2_mean = 0.464999930125687,
  few_letters_X3_mean = -0.224232821975197, few_letters_X4_mean = 0.227624287697716,
  few_letters_X5_mean = 0.0348509105610607, many_letters_X1_mean = -0.0202723585043267,
  many_letters_X2_mean = -0.496103421887354, many_letters_X3_mean = -1.14252694080746,
  many_letters_X4_mean = 1.35630680828917, many_letters_X5_mean = 0.698260253673562,
  some_numbers_1_SPCA = 0.107682587330038, some_numbers_2_SPCA = 0.368306829421735,
  some_numbers_3_SPCA = 0.0960390206033949, some_numbers_4_SPCA = -0.0554822626395495,
  some_numbers_5_SPCA = -0.0274191300888549, many_numbers_X1_mean = -0.136960909487381,
  many_numbers_X2_mean = 0.129211903846762, many_numbers_X3_mean = -0.389538826477393,
  many_numbers_X4_mean = 0.0931307673175262, many_numbers_X5_mean = 0.390391351181884,
  binary_1_SPCA = -0.246333444388948, binary_2_SPCA = 0.0176321450056739
), row.names = c(
  NA,
  -1L
), class = c("data.table", "data.frame"))

test_that("Testing that encoder works", {
  expect_warning(encoder(X = df))

  result <- suppressWarnings(encoder(X = df))
  expect_equal(result_head, result[["encoded"]][1, ])
})
