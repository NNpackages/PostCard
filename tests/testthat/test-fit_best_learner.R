# default_learners
test_that("`default_learners` returns a list of models with class model_spec", {
  learners <- default_learners()
  expect_type(
    learners,
    "list"
  )

  models <- lapply(learners, \(x) x$model)
  all_are_model_spec <- all(sapply(models, \(x) inherits(x, "model_spec")))
  expect_equal(all_are_model_spec, TRUE)
})

# get_preproc_names
test_that("`get_preproc_names` returns vector of model names from wflow_id column", {
  create_data <- function(preproc_names, mod_names = c("name1", "name2")) {
    dat <- data.frame(
      wflow_id = paste(
        preproc_names,
        "_",
        mod_names,
        sep = ""
      )
    )
  }

  preproc_names <- "mod"
  dat <- create_data(preproc_names)
  expect_equal(
    get_preproc_names(dat),
    "mod"
  )

  preproc_names2 <- paste(1:5, "mod", sep = "")
  dat2 <- create_data(preproc_names2)
  expect_equal(
    get_preproc_names(dat2),
    preproc_names2
  )
})

# add_learners
test_that("`add_learners` returns data.frame with correct information", {
  learners <- default_learners()
  learner_names <- names(learners)
  preproc <- list(Y ~ .)
  preproc_name <- "mod"
  names(preproc) <- preproc_name

  out <- add_learners(preproc = preproc,
                      learners = learners)

  expect_type(out, "list")
  expect_equal(colnames(out), c("wflow_id", "info", "option", "result"))

  workflow_names <- paste(preproc_name, learner_names, sep = "_")
  expect_equal(out$wflow_id, workflow_names)

  info_has_columns <- all(sapply(out$info, \(x) colnames(x) == c("workflow", "preproc", "model", "comment")))
  expect_equal(info_has_columns, TRUE)

  options_are_workflow_set_options <- all(sapply(out$option, \(x) inherits(x, "workflow_set_options")))
  expect_equal(options_are_workflow_set_options, TRUE)
})

# get_best_learner
test_that("`get_best_learner` returns a workflow object", {
  dat <- glm_data(
    1+2*x1,
    x1 = rnorm(10),
    response_name = "y"
  )
  cv_folds <- rsample::vfold_cv(dat, v = 2)
  lrnr <- get_best_learner(resamples = cv_folds,
                           learners = default_learners(),
                           formula = "y ~ .",
                           verbose = 0)

  expect_s3_class(lrnr, "workflow")
})

#### Test commented out to avoid running tests for longer than necessary
# test_that("`get_best_learner` can take in a character formula", {
#   dat <- glm_data(
#     1+2*x1,
#     x1 = rnorm(10),
#     response_name = "y"
#   )
#   cv_folds <- rsample::vfold_cv(dat, v = 2)
#   lrnr_chr <- get_best_learner(resamples = cv_folds,
#                                learners = default_learners(),
#                                formula = "y ~ x1",
#                                verbose = 0)
#   expect_s3_class(lrnr_chr, "workflow")
#
#   lrnr_all <- get_best_learner(resamples = cv_folds,
#                                learners = default_learners(),
#                                formula = y ~ .,
#                                verbose = 0)
#   expect_s3_class(lrnr_all, "workflow")
# })

cli::test_that_cli("`get_best_learner` print information when verbose > 0", {
  testthat::local_edition(3)

  dat <- glm_data(
    1+2*x1,
    x1 = rnorm(10),
    response_name = "y"
  )
  cv_folds <- rsample::vfold_cv(dat, v = 2)
  elapsed_time_pattern <- "\\d+\\.?\\d*m?s"
  testthat::expect_snapshot({
    get_best_learner(resamples = cv_folds,
                     learners = default_learners(),
                     formula = "y ~ x1",
                     verbose = 2)
  },
  transform = function(x) gsub(elapsed_time_pattern, "", x))
})

# fit_best_learner
test_that("`fit_best_learner` returns a workflow object", {
  dat <- glm_data(
    1+2*x1,
    x1 = rnorm(10),
    response_name = "y"
  )

  fit <- fit_best_learner(y ~ ., data = dat, verbose = 0)
  expect_s3_class(fit, "workflow")
})
