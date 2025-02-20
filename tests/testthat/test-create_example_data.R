test_that("`family_args` correctly passes arguments", {
  data <- glm_data(
    1+2*x1,
    x1 = rep(1, 10),
    family_args = list(sd = 0)
  )
  expect_equal(sd(data$Y), 0)
})

test_that("Response is created as intended", {
  data <- glm_data(
    1+2*x1,
    x1 = 1:2,
    family_args = list(sd = 0))
  expect_equal(data$Y, c(3, 5))
})

test_that("Variables can be given as data.frame, list and individual variables", {
  ind_args <- withr::with_seed(42, {
    glm_data(
      1+x1+2*x2,
      x1 = 1:2,
      x2 = 4:5
    )
  })

  list_args <- withr::with_seed(42, {
    glm_data(
      1+x1+2*x2,
      list(
        x1 = 1:2,
        x2 = 4:5
      )
    )
  })

  df_args <- withr::with_seed(42, {
    glm_data(
      1+x1+2*x2,
      data.frame(
        x1 = 1:2,
        x2 = 4:5
      )
    )
  })

  expect_equal(ind_args, list_args)
  expect_equal(ind_args, df_args)
})

test_that("family can be given as character, function and call", {
  withr::local_seed(42)
  pois_data <- glm_data(
    1+2*x1,
    x1 = 1:2,
    family = "poisson")
  expect_snapshot(pois_data)

  binom_data <- glm_data(
    1+2*x1,
    x1 = 1:2,
    family = binomial)
  expect_snapshot(binom_data)

  nbinom_data <- glm_data(
    1+2*x1,
    x1 = 1:2,
    family = MASS::negative.binomial(2))
  expect_snapshot(nbinom_data)
})

test_that("Changing `response_name` changes column name in resulting data", {
  data <- glm_data(
    1+2*x1,
    x1 = 1:2,
    response_name = "test"
  )
  expect_true(!is.null(data$test))
})

test_that("Error occurs when no variables are given", {
  expect_error(
    glm_data(1+x1)
  )
})

test_that("Error occurs when trying to use variable in formula that's not defined", {
  expect_error(
    glm_data(1+x1+x2, x1 = rnorm(10))
  )
})

test_that("Error occurs in `check_family` when family is not valid", {
  # Character
  expect_error({
    check_family(family = "testing")
  },
  regexp = "object 'testing' of mode 'function' was not found")

  # Object with NULL family list element
  expect_output({
    expect_error({
      family <- check_family(family = list(A = 1, B = 2))
    },
    regexp = "'family' not recognized")
  })
})
