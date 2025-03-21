#' Find the best learner in terms of RMSE among specified learners using cross validation
#'
#' @inheritParams rctglm
#'
#' @param cv_folds a `numeric` with the number of cross-validation folds used when fitting and
#' evaluating models
#' @param learners a `list` of `tidymodels`
#'
#' @details
#' Ensure data compatibility with the learners.
#'
#' @return a trained `workflow`
#' @export
#'
#' @examples
#' # Generate some synthetic 2-armed RCT data along with historical controls
#' n <- 100
#' dat_rct <- glm_data(
#'   Y ~ 1+2*x1+3*a,
#'   x1 = rnorm(n, 2),
#'   a = rbinom (n, 1, .5),
#'   family = gaussian()
#' )
#' dat_hist <- glm_data(
#'   Y ~ 1+2*x1,
#'   x1 = rnorm(n, 2),
#'   family = gaussian()
#' )
#'
#' # Fit a learner to the historical control data with default learners
#' fit <- fit_best_learner(Y ~ ., data = dat_hist)
#'
#' # Use it fx. to predict the "control outcome" in the 2-armed RCT
#' predict(fit, new_data = dat_rct)
fit_best_learner <- function(data, formula, cv_folds = 5, learners = default_learners(),
                             verbose = options::opt("verbose")) {
  cv_folds <- rsample::vfold_cv(data, v = cv_folds)
  lrnr <- cv_folds %>%
    get_best_learner(learners = learners,
                     formula = formula,
                     verbose = verbose)
  lrnr_fit <- generics::fit(lrnr, data)

  return(lrnr_fit)
}

# Default learners used for searching among
default_learners <- function() {
  list(
    mars = list(
      model = parsnip::mars(
        mode = "regression", prod_degree = 3) %>%
        parsnip::set_engine("earth"),
      grid = NULL
    ),
    lm = list(
      model = parsnip::linear_reg() %>%
        parsnip::set_engine("lm"),
      grid = NULL
    ),
    gbt = list(
      model = parsnip::boost_tree(
        mode = "regression",
        trees = parsnip::tune("trees"),
        tree_depth = parsnip::tune("tree_depth"),
        learn_rate = 0.1
      ) %>%
        parsnip::set_engine("xgboost"),
      grid = data.frame(
        trees = seq(from = 25, to = 500, by = 25),
        tree_depth = 3
      )
    )
  )
}

# set up the pre-processing of the work flow
get_preproc_names <- function(wf) {
  wf %>%
    dplyr::pull(.data$wflow_id) %>%
    sapply(function(x) gsub("\\_.*$", "", x)) %>%
    unique()
}

# function to add learners with the pre-processing
add_learners <- function(preproc, learners) {
  wf <- workflowsets::workflow_set(
    preproc = preproc,
    models = learners %>%
      lapply(function(x) x$model)
  )
  for (learner_name in names(learners)){
    for (preproc_name in get_preproc_names(wf)) {
      wf %<>%
        workflowsets::option_add(
          id = stringr::str_c(preproc_name, "_", learner_name),
          grid = learners[[learner_name]]$grid
        )
    }
  }
  wf
}

# K fold cross validation with recipe -------------------------------------
get_best_learner <- function(
    resamples,
    learners = default_learners(),
    formula,
    verbose = options::opt("verbose")
) {

  if (is.character(formula)) formula <- formula(formula)

  wfs <- add_learners(preproc = list(mod = formula),
                      learners = learners)

  if (verbose >= 1) {
    cli::cli_alert_info("Fitting learners")
    cli::cli_ul(wfs$wflow_id)
  }

  print_model_tuning <- ifelse(verbose >= 2, TRUE, FALSE)
  fit_learners <- wfs %>%
    workflowsets::workflow_map(
      resamples = resamples,
      metrics = yardstick::metric_set(yardstick::rmse),
      verbose = print_model_tuning
    )

  best_learner_name <- fit_learners %>%
    workflowsets::rank_results(rank_metric = 'rmse') %>%
    dplyr::select("wflow_id", "model", ".config", rmse=mean, rank) %>%
    dplyr::filter(dplyr::row_number() == 1) %>%
    dplyr::pull("wflow_id")

  if (verbose >= 1) {
    cli::cli_alert_info("Model with lowest RMSE: {best_learner_name}")
  }

  best_params <- fit_learners %>%
    workflowsets::extract_workflow_set_result(best_learner_name) %>%
    tune::select_best(metric='rmse')

  fit_learners %>%
    tune::extract_workflow(best_learner_name) %>%
    tune::finalize_workflow(best_params)
}
