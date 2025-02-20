test_that("`rctglm` returns an object of same class", {
  n <- 100
  dat_gaus <- glm_data(
    1+1.5*X1+2*A,
    X1 = rnorm(n),
    A = rbinom(n, 1, .5),
    family = gaussian()
  )

  # Fit the model
  ate <- rctglm(formula = Y ~ .,
                group_indicator = A,
                data = dat_gaus,
                family = gaussian)
  expect_s3_class(ate, "rctglm")
  expect_snapshot(estimand(ate))
})

test_that("`rctglm` fails when `group_indicator` is non-binary", {
  n <- 100
  dat_gaus <- glm_data(
    1+1.5*X1+2*A,
    X1 = rnorm(n),
    A = rbinom(n, 1, .5),
    family = gaussian()
  ) %>%
    dplyr::mutate(A_fac = factor(A, levels = 0:1, labels = c("A", "B")))

  # Fit the model
  expect_error(
    {rctglm(formula = Y ~ .,
            group_indicator = A_fac,
            data = dat_gaus,
            family = gaussian)
    },
    regexp = ".*1.*0"
  )
})

test_that("`estimand_fun` argument can be specified as function or character", {
  n <- 100
  dat_gaus <- glm_data(
    1+1.5*X1+2*A,
    X1 = rnorm(n),
    A = rbinom(n, 1, .5),
    family = gaussian()
  )

  ate <- rctglm(formula = Y ~ .,
                group_indicator = A,
                data = dat_gaus,
                family = gaussian,
                estimand_fun = "ate")
  estimand_fun_ate <- gsub("\\s*", "", deparse_fun_body(ate$estimand_fun))
  expect_equal(estimand_fun_ate, "psi1-psi0")

  rr <- rctglm(formula = Y ~ .,
               group_indicator = A,
               data = dat_gaus,
               family = gaussian,
               estimand_fun = "rate_ratio")
  estimand_fun_rr <- gsub("\\s*", "", deparse_fun_body(rr$estimand_fun))
  expect_equal(estimand_fun_rr, "psi1/psi0")

  nonsense_estimand_fun <- function(psi1, psi0) (psi1^2 - sqrt(psi0)) / 2^psi0
  nonsense <- rctglm(formula = Y ~ .,
                     group_indicator = A,
                     data = dat_gaus,
                     family = gaussian,
                     estimand_fun = nonsense_estimand_fun)
  expect_equal(nonsense$estimand_fun, nonsense_estimand_fun)

  # Error when giving character that is not among the defaults
  expect_error(rctglm(formula = Y ~ .,
                      group_indicator = A,
                      data = dat_gaus,
                      family = gaussian,
                      estimand_fun = "test"),
               regexp = 'should be one of "ate", "rate_ratio"')
})

test_that("`estimand_fun_derivX` can be left as NULL or specified manually", {
  n <- 100
  withr::with_seed(42, {
    dat_gaus <- glm_data(
      1+1.5*X1+2*A,
      X1 = rnorm(n),
      A = rbinom(n, 1, .5),
      family = gaussian()
    )
  })

  # Also checking that message is output to console when left as NULL
  expect_snapshot({
    ate_auto <- withr::with_seed(42, {
      rctglm(formula = Y ~ .,
             group_indicator = A,
             data = dat_gaus,
             family = gaussian,
             estimand_fun = "ate",
             verbose = 1)
    })
  })
  ate_man <- withr::with_seed(42, {
    rctglm(formula = Y ~ .,
           group_indicator = A,
           data = dat_gaus,
           family = gaussian,
           estimand_fun = "ate",
           estimand_fun_deriv0 = function(psi1, psi0) -1,
           estimand_fun_deriv1 = function(psi1, psi0) 1)
  })
  expect_equal(ate_auto$estimand, ate_man$estimand)
})
