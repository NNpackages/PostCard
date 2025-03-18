# Modify data to have the same value of a group identifier and then predict
predict_counterfactual_mean <- function(model,
                                        exposure_indicator_name,
                                        group_val,
                                        newdata = NULL,
                                        data = NULL) {
  if (is.null(data)) data <- model$data
  if (is.null(newdata)) newdata <- data
  newdata_same_group_val <- newdata %>%
    dplyr::mutate("{exposure_indicator_name}" := group_val)

  exposure_indicator_in_model <- exposure_indicator_name %in% names(coef(model))
  if (!exposure_indicator_in_model)
    cli::cli_abort("{.arg {exposure_indicator_name}} is not in {.arg {model}}. Specify name of a binary predictor in the {.arg {model}}")
  predict(model,
          type = "response",
          newdata = newdata_same_group_val)
}

predict_counterfactual_means <- function(model,
                                         exposure_indicator_name,
                                         newdata = NULL,
                                         data = NULL) {
  args <- as.list(environment())
  counterfactual_preds <- sapply(
    c(0,1),
    function(x) {
      do.call(
        predict_counterfactual_mean,
        c(args, list(group_val = x))
      )
    },
    simplify = FALSE
  ) %>%
    as.data.frame() %>%
    stats::setNames(c("psi0", "psi1"))

  return(counterfactual_preds)
}

# Default estimand functions
default_estimand_funs <- function(default = c("ate", "rate_ratio")) {
  default <- match.arg(default)

  switch(default,
         ate = function(psi1, psi0) psi1-psi0,
         rate_ratio = function(psi1, psi0) psi1/psi0
  )
}

# Do cross validation splits, fit models to training data and predict on test
# data to obtain out-of-sample predictions
oos_fitted.values_counterfactual <- function(
    data,
    exposure_indicator_name,
    full_model.args_glm,
    cv_variance_folds = 5
) {
  data <- data %>%
    dplyr::mutate(rowname = dplyr::row_number())
  folds <- rsample::vfold_cv(
    data,
    v = cv_variance_folds,
    strata = tidyselect::all_of(exposure_indicator_name)
  )
  train_test_folds <- lapply(
    folds$splits,
    extract_train_test
  )

  args_glm_wo_data <- full_model.args_glm[names(full_model.args_glm) != "data"]

  out <- lapply(train_test_folds, function(x) {
    test_indices <- x$test$rowname
    x <- lapply(x, function(dat) dplyr::select(dat, -rowname))
    args_glm <- args_glm_wo_data
    args_glm$data <- x$train

    model_train <- do.call(glm, args = args_glm)

    preds <- predict_counterfactual_means(
      model = model_train,
      exposure_indicator_name = exposure_indicator_name,
      newdata = x$test
    ) %>%
      dplyr::mutate(rowname = as.numeric(test_indices))
    return(preds)
  }) %>%
    dplyr::bind_rows()

  # Added the rowname before, and sorting now to "collect" the out-of-sample
  # predictions in the same order as data was originally
  out <- out %>%
    dplyr::arrange(.data$rowname)

  return(out)
}


# x of class c("vfold_split", "rsplit"). Result of rsample::vfold_cv
extract_train_test <- function(x) {
  train_data <- x$data[x$in_id, ]
  out_id <- setdiff(1:nrow(x$data), x$in_id)
  test_data <- x$data[out_id, ]
  return(list(train = train_data, test = test_data))
}
