
set.seed(1071)
df <- data.frame(
  y = rnorm(100),
  x_1 = rnorm(100),
  x_2 = rpois(100, 0.5),
  fact_1 = sample(letters[1:3], size = 100, replace = TRUE)
)
dt <- data.table::as.data.table(df)

enc_1 <- assign_encodings(df, "fact_1", method = "dummy")
enc_2 <- assign_encodings(df, "fact_1", method = "mean")

result_head_1 <- structure(list(y = c(
  2.43018001293148, -0.298235294576025, -0.195333037885783,
  0.746759943209612, 1.94078849611945, 0.0210787287558582
), x_1 = c(
  0.173292400299591,
  1.48518279675466, -0.820301101393164, 1.02185693973467, 0.222275087092781,
  0.0507865779555135
), x_2 = c(0L, 0L, 3L, 1L, 0L, 0L), fact_1_b_dummy = c(
  0,
  0, 0, 0, 0, 0
), fact_1_c_dummy = c(0, 0, 0, 0, 0, 0)), class = c(
  "data.table",
  "data.frame"
), row.names = c(NA, -6L))

result_head_2 <- structure(list(y = c(
  2.43018001293148, -0.298235294576025, -0.195333037885783,
  0.746759943209612, 1.94078849611945, 0.0210787287558582
), x_1 = c(
  0.173292400299591,
  1.48518279675466, -0.820301101393164, 1.02185693973467, 0.222275087092781,
  0.0507865779555135
), x_2 = c(0L, 0L, 3L, 1L, 0L, 0L), fact_1_y_mean = c(
  -0.0846637910325627,
  -0.0846637910325627, -0.0846637910325627, -0.0846637910325627,
  -0.0846637910325627, -0.0846637910325627
), fact_1_x_1_mean = c(
  0.0870498068383344,
  0.0870498068383344, 0.0870498068383344, 0.0870498068383344, 0.0870498068383344,
  0.0870498068383344
), fact_1_x_2_mean = c(
  0.379310344827586, 0.379310344827586,
  0.379310344827586, 0.379310344827586, 0.379310344827586, 0.379310344827586
)), class = c("data.table", "data.frame"), row.names = c(
  NA,
  -6L
))


test_that("Fitting an encoder works", {
  encoder_1 <- fit_encoder(X = dt, enc_1)

  expect_equal(length(encoder_1), 3)
  expect_type(encoder_1, "list")
  expect_equal(names(encoder_1), c(
    "encoded",
    "fitted_encoder",
    "fitted_deencoder"
  ))
  expect_true(is.function(encoder_1$fitted_encoder))
  expect_true(is.function(encoder_1$fitted_deencoder))
  expect_equal(result_head_1, head(encoder_1$encoded))

  encoder_2 <- fit_encoder(X = dt, enc_2)

  expect_equal(length(encoder_2), 3)
  expect_type(encoder_2, "list")
  expect_equal(names(encoder_2), c(
    "encoded",
    "fitted_encoder",
    "fitted_deencoder"
  ))
  expect_true(is.function(encoder_2$fitted_encoder))
  expect_true(is.function(encoder_2$fitted_deencoder))
  expect_equal(result_head_2, head(encoder_2$encoded))
})
