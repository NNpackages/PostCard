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

  ate <- rctglm_with_prognosticscore(
    formula = Y ~ .,
    group_indicator = A,
    data = dat_treat,
    family = gaussian(),
    estimand_fun = "ate",
    data_hist = dat_notreat)

  expect_equal(ate$prognostic_info, prog(ate))
  expect_snapshot(prog(ate),
                  transform = function(x) gsub("^<environment:.*>$", "", x))
})

