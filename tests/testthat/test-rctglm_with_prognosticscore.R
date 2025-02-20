test_that("`rctglm_with_prognosticscore` returns object of correct class", {
  withr::local_seed(42)
  # Generate some data
  n <- 100
  b0 <- 1
  b1 <- 1.5
  b2 <- 2
  W1 <- runif(n, min = -2, max = 2)

  dat_treat <- glm_data(
    b0+b1*abs(sin(W1))+b2*A,
    W1 = W1,
    A = rbinom (n, 1, .5)
  )
  dat_notreat <- glm_data(
    b0+b1*abs(sin(W1)),
    W1 = W1
  )

  elapsed_time_pattern <- "\\d+\\.?\\d*m?s"
  expect_snapshot({
    ate <- withr::with_seed(42, {
      rctglm_with_prognosticscore(
        formula = Y ~ .,
        group_indicator = A,
        data = dat_treat,
        family = gaussian(),
        estimand_fun = "ate",
        data_hist = dat_notreat,
        verbose = 2)
    })
  },
  transform = function(x) gsub(elapsed_time_pattern, "", x))

  expect_s3_class(ate, "rctglm_prog")
  expect_s3_class(ate, "rctglm")

  ate_spec_prog <- withr::with_seed(42, {
    rctglm_with_prognosticscore(
      formula = Y ~ .,
      group_indicator = A,
      data = dat_treat,
      family = gaussian(),
      estimand_fun = "ate",
      data_hist = dat_notreat,
      prog_formula = "Y ~ W1")
  })

  expect_equal(est(ate), est(ate_spec_prog))
  expect_snapshot(ate)
})
