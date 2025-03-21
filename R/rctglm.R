# TODO: Add something along the lines of this:
## This method of inference using plug-in estimation and influence functions for the variance produces a
## causal estimate of the estimand, as stated by articles XXXX.
# when article is out

#' Fit GLM and find any estimand (marginal effect) using plug-in estimation with variance estimation using
#' influence functions
#'
#' The procedure uses plug-in-estimation and influence functions to perform robust inference of any specified
#' estimand in the setting of a randomised clinical trial, even in the case of heterogeneous effect of
#' covariates in randomisation groups.
#'
#' @inheritParams stats::glm
#' @inheritParams options
#'
#' @param data an optional data frame, list or environment (or object coercible
#' by as.data.frame to a data frame) containing the variables in the model. If
#' not found in data, the variables are taken from environment(formula), typically
#' the environment from which the function is called.
#' @param formula an object of class "formula" (or one that can be coerced to that class):
#' a symbolic description of the model to be fitted. The details of model specification are
#' given under ‘Details’ in the [glm] documentation.
#' @param exposure_indicator (name of) the *binary* variable in `data` that
#' identifies randomisation groups. The variable is required to be binary to
#' make the "orientation" of the `estimand_fun` clear.
#' @param exposure_prob a `numeric` with the probabiliy of being in
#' "group 1" (rather than group 0) in groups defined by `exposure_indicator`.
#' As a default, the ratio of 1's in data is used.
#' @param estimand_fun a `function` with arguments `psi1` and `psi0` specifying
#' the estimand. Alternative, specify "ate" or "rate_ratio" as a `character`
#' to use one of the default estimand functions. See
#' more details in the "Estimand" section of this documentation.
#' @param estimand_fun_deriv0 a `function` specifying the derivative of `estimand_fun` wrt. `psi0`. As a default
#' the algorithm will use symbolic differentiation to automatically find the derivative from `estimand_fun`
#' @param estimand_fun_deriv1 a `function` specifying the derivative of `estimand_fun` wrt. `psi1`. As a default
#' the algorithm will use symbolic differentiation to automatically find the derivative from `estimand_fun`
#' @param cv_variance a `logical` determining whether to estimate the variance
#' using cross-validation (see details of [rctglm]).
#' @param cv_variance_folds a `numeric` with the number of folds to use for cross
#' validation if `cv_variance` is `TRUE`.
#' @param ... Additional arguments passed to [stats::glm()]
#'
#' @details
#' The procedure assumes the setup of a randomised clinical trial with observations grouped by a binary
#' `exposure_indicator` variable, allocated randomly with probability `exposure_prob`. A GLM is
#' fit and then used to predict the response of all observations in the event that the `exposure_indicator`
#' is 0 and 1, respectively. Taking means of these predictions produce the *counterfactual means*
#' `psi0` and `psi1`, and an estimand `r(psi0, psi1)` is calculated using any specified `estimand_fun`.
#'
#' The variance of the estimand is found by taking the variance of the influence function of the estimand.
#' If `cv_variance` is `TRUE`, then the counterfactual predictions for each observation (which are
#' used to calculate the value of the influence function) is obtained as out-of-sample (OOS) predictions
#' using cross validation with number of folds specified by `cv_variance_folds`. The cross validation splits
#' are performed using stratified sampling with `exposure_indicator` as the `strata` argument in [rsample::vfold_cv].
#'
#' @section Estimands:
#' As noted in the description, `psi0` and `psi1` are the counterfactual means found by prediction using
#' a fitted GLM in the binary groups defined by `exposure_indicator`.
#'
#' Default estimand functions can be specified via `"ate"` (which uses the function
#' `function(psi1, psi0) psi1-psi0`) and `"rate_ratio"` (which uses the function
#' `function(psi1, psi0) psi1/psi0`). See more information on specifying the `estimand_fun`
#' in section
#' [Specifying any estimand](https://nnpackages.github.io/PostCard/articles/more-details.html#specifying-any-estimand)
#' in the vignette `vignette("more-details")`.
#'
#' As a default, the `Deriv` package is used to perform symbolic differentiation to find the derivatives of
#' the `estimand_fun`.
#'
#' @return `rctglm` returns an object of class inheriting from `"rctglm"`.
#'
#' The function [estimand] (or short-hand version [est]) can be used to extract
#' a `data.frame` with an estimated value and standard error of the estimand.
#'
#' A method for the generic [coef] has been added for `rctglm`
#' (i.e., [coef.rctglm]), which uses the method `coef.glm` to extract coefficient
#' information from the underlying `glm` fit in the procedure.
#'
#' An object of class `rctglm` is a list containing the following components:
#' -   `estimand`: A `data.frame` with plug-in estimate of estimand, standard
#' error (SE) estimate and variance estimate of estimand
#' -   `estimand_fun`: The `function` used to obtain an estimate of the estimand
#' from counterfactual means
#' -   `means_counterfactual`: A `data.frame` with counterfactual means `psi0` and `psi1`
#' -   `fitted.values_counterfactual`: A `data.frame` with counterfactual mean
#' values, obtained by transforming the linear predictors for each group
#' by the inverse of the link function.
#' -   `glm`: A `glm` object returned from running [stats::glm] within the procedure
#' -   `call`: The matched `call`
#' @export
#'
#' @examples
#' # Generate some data to showcase example
#' n <- 100
#' exposure_prob <- .5
#'
#' dat_gaus <- glm_data(
#'   Y ~ 1+1.5*X1+2*A,
#'   X1 = rnorm(n),
#'   A = rbinom(n, 1, exposure_prob),
#'   family = gaussian()
#' )
#'
#' # Fit the model
#' ate <- rctglm(formula = Y ~ .,
#'               exposure_indicator = A,
#'               exposure_prob = exposure_prob,
#'               data = dat_gaus,
#'               family = gaussian)
#'
#' # Pull information on estimand
#' estimand(ate)
#'
#' ## Another example with different family and specification of estimand_fun
#' dat_binom <- glm_data(
#'   Y ~ 1+1.5*X1+2*A,
#'   X1 = rnorm(n),
#'   A = rbinom(n, 1, exposure_prob),
#'   family = binomial()
#' )
#'
#' rr <- rctglm(formula = Y ~ .,
#'               exposure_indicator = A,
#'               exposure_prob = exposure_prob,
#'               data = dat_binom,
#'               family = binomial(),
#'               estimand_fun = "rate_ratio")
#'
#' odds_ratio <- function(psi1, psi0) (psi1*(1-psi0))/(psi0*(1-psi1))
#' or <- rctglm(formula = Y ~ .,
#'               exposure_indicator = A,
#'               exposure_prob = exposure_prob,
#'               data = dat_binom,
#'               family = binomial,
#'               estimand_fun = odds_ratio)
#'
#'
rctglm <- function(formula,
                   exposure_indicator,
                   exposure_prob,
                   family,
                   data,
                   estimand_fun = "ate",
                   estimand_fun_deriv0 = NULL, estimand_fun_deriv1 = NULL,
                   cv_variance = TRUE,
                   cv_variance_folds = 10,
                   verbose = options::opt("verbose"),
                   ...
) {

  # TODO: Right now, estimand_fun needs to have arguments with 0 and 1 in them, and the exposure_indicator
  # needs to take values 0 and 1. Generalise this to work for factors and be able to set reference level

  exposure_indicator <- rlang::enquo(exposure_indicator)
  args <- as.list(environment())
  cal <- match.call()
  formula <- check_formula(formula)

  ind_expr <- rlang::quo_get_expr(exposure_indicator)
  called_within_prognosticscore <- ind_expr == "exposure_indicator"
  if (called_within_prognosticscore) {
    exposure_indicator_name <- as.character(rlang::quo_get_expr(rlang::eval_tidy(exposure_indicator)))
  } else {
    exposure_indicator_name <- as.character(ind_expr)
  }

  group_vals <- dplyr::pull(data, tidyselect::all_of(exposure_indicator_name))
  group_vals_unique <- unique(group_vals)
  if (!all(c(0,1) %in% group_vals)) cli::cli_abort("{.var exposure_indicator} column can only have 1's and 0's")

  exposure_prob_is_num <- inherits(exposure_prob, "numeric")
  exposure_in_range <- exposure_prob > 0 & exposure_prob < 1
  if (!exposure_prob_is_num | !exposure_in_range)
    cli::cli_abort("`exposure_prob` needs to be a probability, i.e. a numeric between 0 and 1")
  # if (is.null(exposure_prob)) {
  #   exposure_prob <- mean(group_vals)
  #   if (verbose >= 1)
  #     cli::cli_alert_info("Setting the group allocation probability {.var exposure_prob} as the mean of column {.var {exposure_indicator_name}} in data: {exposure_prob}")
  # }

  if (is.character(estimand_fun)) estimand_fun <- default_estimand_funs(estimand_fun)

  if (is.null(estimand_fun_deriv0) | is.null(estimand_fun_deriv1)) {
    args01 <- get01args(fun = estimand_fun)

    if (verbose >= 1) cli::cli_h2("Symbolic differentiation of estimand function")
    if (is.null(estimand_fun_deriv0)) {
      estimand_fun_deriv0 <- print_symbolic_differentiation(
        arg = args01[["arg0"]],
        fun = estimand_fun,
        add_string = "Alternatively, specify the derivative through the argument {.var estimand_fun_deriv0}\n",
        verbose = verbose)
    }
    if (is.null(estimand_fun_deriv1)) {
      estimand_fun_deriv1 <- print_symbolic_differentiation(
        arg = args01[["arg1"]],
        fun = estimand_fun,
        add_string = "Alternatively, specify the derivative through the argument {.var estimand_fun_deriv1}\n",
        verbose = verbose)
    }
  }

  args_glm <- c(args[names(args) %in% names(formals(glm))], list(...))
  model <- do.call(glm, args = args_glm)

  response_var <- model$y
  exposure_indicator_var <- data %>%
    dplyr::pull(tidyselect::all_of(exposure_indicator_name))

  full_model_fitted.values_counterfactual <- predict_counterfactual_means(
    model = model,
    exposure_indicator_name = exposure_indicator_name
  )
  full_model_means_counterfactual <- colMeans(full_model_fitted.values_counterfactual)
  full_model_means_counterfactual0 <- as.numeric(full_model_means_counterfactual["psi0"])
  full_model_means_counterfactual1 <- as.numeric(full_model_means_counterfactual["psi1"])

  estimate_estimand <- estimand_fun(
    psi1 = full_model_means_counterfactual1,
    psi0 = full_model_means_counterfactual0
  )

  # If cv_variance then use out-of-sample counterfactual predictions, otherwise
  # use predictions from full model fitted to all data
  preds_for_variance <- if (cv_variance) {
    oos_fitted.values_counterfactual(
      data = data,
      exposure_indicator_name = exposure_indicator_name,
      full_model.args_glm = args_glm,
      cv_variance_folds = cv_variance_folds
    )
  } else {
    full_model_fitted.values_counterfactual
  }

  if_marginaleffect_val <- if_marginaleffect(
    response_variable = response_var,
    exposure_indicator = exposure_indicator_var,
    exposure_prob = exposure_prob,
    counterfactual_pred0 = preds_for_variance$psi0,
    counterfactual_pred1 = preds_for_variance$psi1,
    counterfactual_mean0 = full_model_means_counterfactual0,
    counterfactual_mean1 = full_model_means_counterfactual1,
    estimand_fun_deriv0 = estimand_fun_deriv0,
    estimand_fun_deriv1 = estimand_fun_deriv1)

  var_estimand <- as.numeric(var(if_marginaleffect_val))
  se_estimand <- sqrt(var_estimand/nrow(data))

  data_estimand <- data.frame(Estimate = estimate_estimand,
                              `Std. Error` = se_estimand,
                              check.names = FALSE)

  out <- list(
    estimand = data_estimand,
    estimand_fun = estimand_fun,
    means_counterfactual = full_model_means_counterfactual,
    fitted.values_counterfactual = full_model_fitted.values_counterfactual,
    glm = model,
    call = cal
  )

  return(structure(out, class = c("rctglm", class(out))))
}
