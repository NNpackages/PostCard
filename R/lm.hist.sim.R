#' ANCOVA models using historical data in a simulations study
#'
#' @description
#' The function estimates the ATE, std. error of ATE, coverage, power, type1 error
#' rate, and approximations of power for a pair of data sets in data.list. The output
#' of the function is a data set with each row being the estimated entities on
#' each combination of historical and current RCT data from the data.list object.
#' Thus, column means can be taken to get averaged results across the different
#' data sets.
#'
#' @param data.list          A list of list elements 1, 2, 3,..., N.sim each with elements $hist and $rct, which are both data.frames being historical and current RCT data sets, respectively. A $hist_test element of the list can be provided for prospective power calculations. Fx. this object could be the output of sim.lm.
#' @param method             Method for using historical data. Options: None, PROCOVA, PSM, Oracle0, Oracle. None refers to ANOVA (for adj.covs = NULL) and ANCOVA (for adj.covs specified). For oracle and oracle0 the data.list objects should be generated using sim.lm.
#' @param margin             Superiority margin (for non-inferiority margin, a negative value can be provided).
#' @param alpha              Significance level. Due to regulatory guidelines when using a one-sided test, half the specified significance level is used. Thus, for standard alpha = .05, a significance level of 0.025 is used.
#' @param outcome.var        Character with the name of the outcome variable in both the $rct and $hist data set.
#' @param treatment.var      Character with the name of the treatment indicator in both the $rct and $hist data set. Notice that the treatment variable should be an indicator with treatment == 1 and control == 0.
#' @param adj.covs           Character vector with names of the covariates to adjust for as raw covariates in the ANCOVA model for estimating the ATE. Make sure that categorical variables are considered as factors.
#' @param interaction        Logical value, that determines whether to model interaction effects between covariates and treatment indicator when estimating the ATE. For method = "PROCOVA", the prognostic score is regarded as an additional covariate and thus the interaction between the prognostic score and the treatment indicator is included.
#' @param est.power          Logical value. If set to TRUE, prospective power calculation is carried out based on theoretical non-centrality parameter as well as Guenther-Schouten approximations, using historical data "hist" and "hist_test" provided in data.list. The entities sigma, rho, and R2 are calculated using the historical data, and these can not be specified by the user. Look at NC_power or GS_power if you want a power calculation based on user specified entities. For method="PROCOVA", only "hist_test" data is used since "hist" data is used for training the model. In addition, the necessary entities for these calculations are outputted.
#' @param ATE                The average treatment effect. If est.power == TRUE this value is the minimum effect size that we should be able to detect. If data was simulated using the sim.lm function the value is set equal to the ATE attribute from the data set.
#' @param pred.model         Model object which should be an R function of the historical data that fits a prediction model based on the baseline covariates. This is only needed for method == "PROCOVA". The model object obtained from the function should be a valid argument for \link[stats]{predict}, where newdata = data.list$rct and with the treatment variable equal to w and outcome variable equal to y. Note that if there is treatment patients in the historical data the prediction model should include treatment.var as a baseline covariate in order to predict \eqn{(E[Y(0)|X], E[Y(1)|X])}.
#' @param B                  Only relevant for method = PSM. Number of bootstraps for estimating bias between HC and CC groups.
#' @param L2                 Logical value. Only relevant if method = "PROCOVA" is specified. If set to TRUE, average squared difference between true and estimated prognostic scores are estimated as column "L2". The difference is estimated on elements in the rct data set and can only be used for data.list generated by sim.lm. Irrelevant for Oracle estimators (since these use the true prognostic score).
#' @param parallel           Logical value. If TRUE the calculations are done using parallelisation using future::plan(future::multicore), which resolves futures asynchronously (in parallel) in separate forked R processes running in the background on the same machine. This is NOT supported on Windows. Reason for not using future::multisession is that this creates and error due to the crit.val.t not being saved, probably stemming from a bug in future::multisession.
#' @param workers            Number of cores to use for parallelisation. Only relevant if parallel = TRUE.
#' @param ...                Additional arguments for sandwich::vcovHC for HC estimation of the standard error. See \link[sandwich]{vcovHC} for more information.
#'
#'
#' @details
#' The prospective power estimations are determined by the NC_power and GS_power functions. Look at the details of these to see
#' specifically how the power is determined. If interaction = TRUE and there is historical treatment group participants the interaction
#' terms are included in the calculation of R2, otherwise this is not included and hence the power is conservatively estimated.
#'
#' The coverage is calculated as the proportion of times the true ATE was inside the confidence intervals
#'
#' \deqn{\left[\widehat{\mathrm{ATE}}_i- t_{1-\alpha/2, n-k}\sqrt{\mathbb{V}\mathrm{ar}\left(\widehat{\mathrm{ATE}}_i\right)},\;\; \widehat{\mathrm{ATE}}_i+t_{1-\alpha/2, n-k}\sqrt{\mathbb{V}\mathrm{ar}\left(\widehat{\mathrm{ATE}}_i\right)}\right]}
#'
#' for i=1,2,3,...,N.sim, where \eqn{\widehat{\mathrm{ATE}}_i} and \eqn{\mathbb{V}\mathrm{ar}\left(\widehat{\mathrm{ATE}}_i\right)} are the ATE and
#' variance estimates from the ith data set, and \eqn{t_{1-\alpha/2, n-k}} is the \eqn{(1-\alpha/2)\%}-quantile of the t-distribution with degrees of
#' freedom equal to the sample size n minus the number of columns in the design matrix. Using the \eqn{(1-\alpha/2)\%}-quantile, we specify a
#' significance level of \eqn{(\alpha/2)\%} for the one-sided superiority test that we want to perform, which corresponds to specification
#' of a \eqn{\alpha\%} significance level for a two-sided test, and we would therefore expect an estimated coverage of \eqn{(1-\alpha)\%}.
#'
#' In order to empirically estimate the probability of correctly rejecting this null hypothesis (the power) we evaluate
#' if (estimate - margin)/std.err > crit.val.t for each pair of RCT and historical data. This TRUE/FALSE variable is
#' given in the t.test output. The power is estimated as the proportion of times t.test == TRUE.
#'
#' In order to empirically estimate the probability of mistakenly rejecting the null hypothesis (the type I
#' error probability), we alter the simulated data by subtracting \eqn{\mathrm{ATE}-margin} from the outcome variable for
#' patients in the treatment group, such that the new \eqn{\mathrm{ATE}} was equal to the superiority margin (the case of
#' correct null hypothesis which has largest probability of rejection). Subtracting \eqn{\mathrm{ATE}-margin} from the
#' outcome variable for patients in the treatment group corresponds to shifting the estimated treatment effect by
#' \eqn{\mathrm{ATE}-margin}. The type I error probability is then estimated by calculating the number of times we
#' (incorrectly) rejected the null hypothesis from the $t$-test statistic.
#'
#' @return
#' The function returns a data set where each row is the estimated entities on each combination of historical and current RCT data. That means
#' that there will be N.sim rows if data was simulated using the sim.lm function. Thus, column means can be taken to get averaged results across the different data sets.
#'
#' @examples
#' data <- sim.lm(N.sim = 5, N.hist.control = 100, N.hist.treatment = 100,
#'               N.control = 50, N.treatment = 50)
#'
#' lm.hist.sim(data, workers = 3)
#'
#' @export
#'
#'
#' @importFrom future plan availableCores multicore
#' @importFrom future.apply future_lapply
#' @importFrom magrittr "%>%"
#' @importFrom dplyr bind_rows across case_when
#' @importFrom stats formula setNames predict model.matrix qt lm
#'
lm.hist.sim <- function(data.list,
                        method = "None",
                        margin = 0,
                        alpha = .05,
                        outcome.var = "y",
                        treatment.var = "w",
                        adj.covs = NULL,
                        interaction = FALSE,
                        pred.model = NULL,
                        B = 100,
                        est.power = FALSE,
                        ATE = NULL,
                        L2 = FALSE,
                        parallel = TRUE,
                        workers = future::availableCores(),
                        ...) {

  ####### Check if variables are defined correctly ##########
  stopifnot(is.character(method), length(method) == 1L,
            is.numeric(margin), length(margin) == 1L,
            is.numeric(alpha), length(alpha) == 1L,
            is.character(outcome.var), length(outcome.var) == 1L,
            is.character(treatment.var), length(treatment.var) == 1L,
            is.character(adj.covs) | is.null(adj.covs),
            is.logical(interaction),
            is.function(pred.model) | is.null(pred.model),
            is.numeric(B), length(B) == 1L,
            is.logical(est.power), length(est.power) == 1L,
            is.numeric(ATE) | is.null(ATE),
            is.logical(L2), length(L2) == 1L,
            is.numeric(workers), length(workers) == 1L)

  ####### Preliminary setting of variables and adjustment of data sets ##########
  method <- tolower(method)
  N.sim <- length(data.list)

  if (!is.null(attr(data.list, "ATE"))) {
    ATE <- attr(data.list, "ATE")
  }
  if (is.null(ATE) & is.null(attr(data.list, "ATE"))) {
    stop("Specify a value for ATE.")
  }



  ######## ANOVA or ANCOVA model without use of historical data #########
  if (method == "none") {
    n <- data.list[[1]]$rct %>% nrow()
    n1 <- data.list[[1]]$rct[data.list[[1]]$rct$w == 1, ] %>% nrow()
    n0 <- data.list[[1]]$rct[data.list[[1]]$rct$w == 0, ] %>% nrow()
    r <- n1/n0

    none.sim <- function(k) {
      mod <- lm.procova(data.list[[k]], method = "None", margin = margin, alpha = alpha, outcome.var = outcome.var,
                        treatment.var = treatment.var, adj.covs = adj.covs, interaction = interaction)

      s1 <- lmtest::coeftest(mod, vcov = sandwich::vcovHC(mod, ...))
      estimate <- s1["w", "Estimate"]
      std.err <- s1["w", "Std. Error"]
      crit.val.t <- mod$test_margin$crit.val.t
      coverage <- (ATE <= estimate + crit.val.t * std.err & ATE >= estimate - crit.val.t * std.err)
      MSE <- (ATE - estimate)^2
      test_stat <- ((estimate - (ATE - margin)) - margin)/std.err
      type1.err <- test_stat > crit.val.t

      res <- c(estimate, std.err, mod$test_margin[2:3], coverage, MSE, type1.err) %>% stats::setNames(nm = c("estimate", "std.err", "test_stat", "power", "coverage", "MSE", "type1.err"))

      if (est.power) {
        if (!is.null(data.list[[k]]$hist_test)) {
          hist <- rbind(data.list[[k]]$hist, data.list[[k]]$hist_test)
        } else {
          hist <- data.list[[k]]$hist
        }
        prelim <- power.ancova(data.hist = hist, outcome.var = outcome.var, treatment.var = treatment.var,
                               adj.covs = adj.covs, interaction = interaction, n = n, r = r, ATE = ATE, margin = margin,
                               alpha = alpha)
        res <- c(res, prelim)
      }
      res
    }


    if (parallel) {
      oplan <-  future::plan(future::multicore, workers = workers)
      on.exit(plan(oplan))
      out <- future.apply::future_lapply(X = 1:N.sim, FUN = none.sim)
    } else {
      out <- lapply(1:N.sim, FUN = none.sim)
    }

    out <- dplyr::bind_rows(out)
  }

  ######## PROCOVA model with use of historical data #########
  if (method == "procova") {
    if (is.null(pred.model)) {
      stop("Specify a pred.model")
    }

    n <- data.list[[1]]$rct %>% nrow()
    n1 <- data.list[[1]]$rct[data.list[[1]]$rct$w == 1, ] %>% nrow()
    n0 <- data.list[[1]]$rct[data.list[[1]]$rct$w == 0, ] %>% nrow()
    r <- n1/n0


    procova.sim <- function(k) {
      mod <- lm.procova(data.list[[k]], method = "PROCOVA", margin = margin, alpha = alpha, outcome.var = outcome.var,
                        treatment.var = treatment.var, adj.covs = adj.covs, interaction = interaction, pred.model = pred.model)

      s1 <- lmtest::coeftest(mod, vcov = sandwich::vcovHC(mod, ...))
      estimate <- s1["w", "Estimate"]
      std.err <- s1["w", "Std. Error"]
      crit.val.t <- mod$test_margin$crit.val.t
      coverage <- (ATE <= estimate + crit.val.t * std.err & ATE >= estimate - crit.val.t * std.err)
      MSE <- (ATE - estimate)^2
      test_stat <- ((estimate - (ATE - margin)) - margin)/std.err
      type1.err <- test_stat > crit.val.t

      res <- c(estimate, std.err, mod$test_margin[2:3], coverage, MSE, type1.err) %>% stats::setNames(nm = c("estimate", "std.err", "test_stat", "power", "coverage", "MSE", "type1.err"))

      if (est.power) {
        if (is.null(data.list[[k]]$hist_test)) {
          stop("Additional list of historical data hist_test should be provided in data.list")
        } else {
          hist <- data.list[[k]]$hist_test
          hist$pred <- stats::predict(attr(mod, "prediction_model"), new_data = hist)
        }
        prelim <- power.ancova(data.hist = hist, outcome.var = outcome.var, treatment.var = treatment.var,
                               adj.covs = c(adj.covs, "pred"), interaction = interaction, n = n, r = r, ATE = ATE, margin = margin,
                               alpha = alpha)
        res <- c(res, prelim)
      }

      if (L2) {
        N.covs <- attr(data.list, "N.covs")
        coefs <- attr(data.list, "coefs")
        n1_hist <- data.list[[k]]$hist[data.list$hist$w == 1, ] %>% nrow()

        if (n1_hist == 0) {
          # Calculating true prognostic score (as for pred.model == "oracle0")
          rct0 <- data.list[[k]]$rct %>% dplyr::mutate(w = 0)
          rct0$pred <- stats::predict(attr(mod, "prediction_model"), new_data = rct0)
          X <- stats::model.matrix(stats::formula(paste0("y ~ -1 +", paste0("(", paste0("x", 1:N.covs, collapse = "+"), ")^2"),
                                                         "+", paste0("I(x", 1:N.covs, "^2)", collapse = "+"))),
                                   data = rct0)
          rct0$progscore <- X %*% c(rep(coefs[2], N.covs), rep(coefs[1], N.covs), rep(coefs[1], ncol(X) - (2*N.covs)))
          prelim <- c(colMeans((rct0$pred - rct0$progscore)^2)) %>% stats::setNames(nm = c("L2"))
          res <- c(res, prelim)
        } else {
          # Predict as if they receive treatment
          rct <- data.list[[k]]$rct %>% dplyr::mutate("w" = 1)
          rct$pred1 <- stats::predict(attr(mod, "prediction_model"), new_data = rct)

          ## Treatment estimate with procova
          X <- stats::model.matrix(stats::formula(paste0("y ~ 1 +", paste0("(", paste0("x", 1:N.covs, collapse = "+"), ")^2"),
                                                         "+", paste0("I(x", 1:N.covs, "^2)", collapse = "+"),
                                                         "+", paste0("I(x", 1:N.covs, "*w)", collapse = "+"))),
                                   data = rct)
          rct$progscore1 <- X %*% c(ATE, rep(coefs[2], N.covs), rep(coefs[1], N.covs), rep(coefs[3], N.covs), rep(coefs[1], ncol(X) - (3*N.covs + 1)))
          L2_1 <- colMeans((rct$pred1 - rct0$progscore1)^2)

          # Predict as if they receive control
          rct <- rct %>% dplyr::mutate("w" = 0)
          rct$pred0 <- stats::predict(attr(mod, "prediction_model"), new_data = rct)
          X <- stats::model.matrix(stats::formula(paste0("y ~ -1 +", paste0("(", paste0("x", 1:N.covs, collapse = "+"), ")^2"),
                                                         "+", paste0("I(x", 1:N.covs, "^2)", collapse = "+"))),
                                   data = rct)
          rct0$progscore0 <- X %*% c(rep(coefs[2], N.covs), rep(coefs[1], N.covs), rep(coefs[1], ncol(X) - (2*N.covs)))
          L2_0 <- colMeans((rct$pred0 - rct0$progscore0)^2)

          prelim <- c(L2_1, L2_0) %>% stats::setNames(nm = c("L2_1", "L2_0"))
          res <- c(res, prelim)
        }
      }
      res
    }

    if (parallel) {
      oplan <-  future::plan(future::multicore, workers = workers)
      on.exit(plan(oplan))
      out <- future.apply::future_lapply(X = 1:N.sim, future.seed = TRUE, FUN = procova.sim)
    } else {
      out <- lapply(1:N.sim, FUN = procova.sim)
    }

    out <- dplyr::bind_rows(out)
  }


  ######## PSM model with use of historical data #########
  if (method == "psm") {
    n <- data.list[[1]]$rct %>% nrow()
    n.adj <- length(adj.covs) + ifelse(interaction, (length(adj.covs) + 2), 0)
    n1 <- data.list[[1]]$rct[data.list[[1]]$rct$w == 1, ] %>% nrow()
    n0 <- data.list[[1]]$rct[data.list[[1]]$rct$w == 0, ] %>% nrow()
    r <- n1/n0

    psm.sim <- function(k) {
      mod <- lm.psm(data.list[[k]], margin = margin, alpha = alpha, outcome.var = outcome.var,
                    treatment.var = treatment.var, adj.covs = adj.covs, interaction = interaction, B = B)
      estimate <- mod$estimate
      std.err <- mod$std.err
      crit.val.t <- mod$crit.val.t
      coverage <- (ATE <= estimate + crit.val.t * std.err & ATE >= estimate - crit.val.t * std.err)
      MSE <- (ATE - estimate)^2
      test_stat <- ((estimate - (ATE - margin)) - margin)/std.err
      type1.err <- test_stat > crit.val.t

      res <- c(estimate, std.err, mod[4:5], coverage, MSE, type1.err) %>% stats::setNames(nm = c("estimate", "std.err", "test_stat", "power", "coverage", "MSE", "type1.err"))

      if (est.power) {
        if (is.null(data.list[[k]]$hist_test)) {
          stop("Additional list of historical data hist_test should be provided in data.list")
        } else {
          hist <- data.list[[k]]$hist_test
        }
        prelim <- power.ancova(data.hist = hist, outcome.var = outcome.var, treatment.var = treatment.var,
                               adj.covs = adj.covs, interaction = interaction, n = 2*n1, r = r, ATE = ATE, margin = margin,
                               alpha = alpha)
        res <- c(res, prelim)
      }
      res
    }


    if (parallel) {
      oplan <-  future::plan(future::multicore, workers = workers)
      on.exit(plan(oplan))
      out <- future.apply::future_lapply(X = 1:N.sim, future.seed = TRUE, FUN = psm.sim)
    } else {
      out <- lapply(1:N.sim, FUN = psm.sim)
    }

    out <- dplyr::bind_rows(out)
  }

  ######## oracle0 model #########
  if (method == "oracle0") {
    n <- data.list[[1]]$rct %>% nrow()
    n1 <- data.list[[1]]$rct[data.list[[1]]$rct$w == 1, ] %>% nrow()
    n0 <- data.list[[1]]$rct[data.list[[1]]$rct$w == 0, ] %>% nrow()
    r <- n1/n0
    N.covs <- attr(data.list, "N.covs")
    coefs <- attr(data.list, "coefs")
    crit.val.t <- stats::qt(1 - alpha/2, nrow(data.list[[1]]$rct) - 3 - length(adj.covs) - ifelse(interaction, (length(adj.covs) + 1), 0))
    formula.procova  <- stats::formula(paste0("y ~ w + pred +", dplyr::case_when(
      is.null(adj.covs) & !interaction ~ "1",
      !is.null(adj.covs) & interaction ~ paste0(paste0(adj.covs, collapse = " + "), "+", "pred*w", "+", paste0(adj.covs, "*w", collapse = " + ")),
      is.null(adj.covs) & interaction ~ "w*pred",
      T ~ paste0(adj.covs, collapse = " + "))))


    oracle0.sim <- function(k) {
      rct <- data.list[[k]]$rct
      rct0 <- rct %>% dplyr::mutate(w = 0)
      rct.dm <- data.list[[k]]$rct %>%
        dplyr::rename(w = treatment.var, y = outcome.var) %>%
        dplyr::mutate(dplyr::across(!c("w", "y"), function(x) x - mean(x)))

      X <- stats::model.matrix(stats::formula(paste0("y ~ -1 +", paste0("(", paste0("x", 1:N.covs, collapse = "+"), ")^2"),
                                                     "+", paste0("I(x", 1:N.covs, "^2)", collapse = "+"))),
                               data = rct0)
      rct.dm$pred <- X %*% c(rep(coefs[2], N.covs), rep(coefs[1], N.covs), rep(coefs[1], ncol(X) - (2*N.covs)))
      rct.dm$pred <- rct.dm$pred - mean(rct.dm$pred)

      mod_procova <- stats::lm(formula = formula.procova, data = rct.dm)

      s1 <- lmtest::coeftest(mod_procova, vcov = sandwich::vcovHC(mod_procova, ...))
      estimate <- s1["w", "Estimate"]
      std.err <- s1["w", "Std. Error"]
      coverage <- (ATE <= estimate + crit.val.t * std.err & ATE >= estimate - crit.val.t * std.err)
      MSE <- (ATE - estimate)^2
      test_stat <- (estimate - margin)/std.err
      power <- test_stat > crit.val.t
      test_stat1 <- ((estimate - (ATE - margin)) - margin)/std.err
      type1.err <- test_stat1 > crit.val.t

      res <- c(estimate, std.err, test_stat, power, coverage, MSE, type1.err) %>% stats::setNames(nm = c("estimate", "std.err", "test_stat", "power", "coverage", "MSE", "type1.err"))

      if (est.power) {
        if (!is.null(data.list[[k]]$hist_test)) {
          hist <- rbind(data.list[[k]]$hist, data.list[[k]]$hist_test)
        } else {
          hist <- data.list[[k]]$hist
        }

        X <- stats::model.matrix(stats::formula(paste0("y ~ -1 +", paste0("(", paste0("x", 1:N.covs, collapse = "+"), ")^2"),
                                                       "+", paste0("I(x", 1:N.covs, "^2)", collapse = "+"))),
                                 data = hist)
        hist$pred <- X %*% c(rep(coefs[2], N.covs), rep(coefs[1], N.covs), rep(coefs[1], ncol(X) - (2*N.covs)))

        prelim <- power.ancova(data.hist = hist, outcome.var = outcome.var, treatment.var = treatment.var,
                               adj.covs = c(adj.covs, "pred"), interaction = interaction, n = n, r = r, ATE = ATE, margin = margin,
                               alpha = alpha)
        res <- c(res, prelim)
      }
      res
    }


    if (parallel) {
      oplan <-  future::plan(future::multicore, workers = workers)
      on.exit(plan(oplan))
      out <- future.apply::future_lapply(X = 1:N.sim, future.seed = TRUE, FUN = oracle0.sim)
    } else {
      out <- lapply(1:N.sim, FUN = oracle0.sim)
    }

    out <- dplyr::bind_rows(out)
  }


  ######## oracle model with use of historical data #########
  if (method == "oracle") {
    n <- data.list[[1]]$rct %>% nrow()
    n1 <- data.list[[1]]$rct[data.list[[1]]$rct$w == 1, ] %>% nrow()
    n0 <- data.list[[1]]$rct[data.list[[1]]$rct$w == 0, ] %>% nrow()
    r <- n1/n0
    N.covs <- attr(data.list, "N.covs")
    coefs <- attr(data.list, "coefs")
    crit.val.t <- stats::qt(1 - alpha/2, nrow(data.list[[1]]$rct) - 4 - length(adj.covs) - ifelse(interaction, (length(adj.covs) + 2), 0))
    formula.procova  <- stats::formula(paste0("y ~ w + pred1 + pred0 +", dplyr::case_when(
      is.null(adj.covs) & !interaction ~ "1",
      !is.null(adj.covs) & interaction ~ paste0(paste0(adj.covs, collapse = " + "), "+", paste0("pred", 0:1, "*w", collapse = "+"), "+", paste0(adj.covs, "*w", collapse = " + ")),
      is.null(adj.covs) & interaction ~ paste0("pred", 0:1, "*w", collapse = "+"),
      T ~ paste0(adj.covs, collapse = " + "))))

    oracle.sim <- function(k) {
      rct <- data.list[[k]]$rct
      rct0 <- rct %>% dplyr::mutate(w = 0)
      rct1 <- rct %>% dplyr::mutate(w = 1)
      rct.dm <- data.list[[k]]$rct %>%
        dplyr::rename(w = treatment.var, y = outcome.var) %>%
        dplyr::mutate(dplyr::across(!c("w", "y"), function(x) x - mean(x)))

      X <- stats::model.matrix(stats::formula(paste0("y ~ 1 +", paste0("(", paste0("x", 1:N.covs, collapse = "+"), ")^2"),
                                                     "+", paste0("I(x", 1:N.covs, "^2)", collapse = "+"),
                                                     "+", paste0("I(x", 1:N.covs, "*w)", collapse = "+"))),
                               data = rct1)
      rct.dm$pred1 <- X %*% c(ATE, rep(coefs[2], N.covs), rep(coefs[1], N.covs), rep(coefs[3], N.covs), rep(coefs[1], ncol(X) - (3*N.covs + 1)))
      rct.dm$pred1 <- rct.dm$pred1 - mean(rct.dm$pred1)
      X <- stats::model.matrix(stats::formula(paste0("y ~ -1 +", paste0("(", paste0("x", 1:N.covs, collapse = "+"), ")^2"),
                                                     "+", paste0("I(x", 1:N.covs, "^2)", collapse = "+"))),
                               data = rct0)
      rct.dm$pred0 <- X %*% c(rep(coefs[2], N.covs), rep(coefs[1], N.covs), rep(coefs[1], ncol(X) - (2*N.covs)))
      rct.dm$pred0 <- rct.dm$pred0 - mean(rct.dm$pred0)

      mod_procova <- stats::lm(formula = formula.procova, data = rct.dm)

      s1 <- lmtest::coeftest(mod_procova, vcov = sandwich::vcovHC(mod_procova, ...))
      estimate <- s1["w", "Estimate"]
      std.err <- s1["w", "Std. Error"]
      coverage <- (ATE <= estimate + crit.val.t * std.err & ATE >= estimate - crit.val.t * std.err)
      MSE <- (ATE - estimate)^2
      test_stat <- (estimate - margin)/std.err
      power <- test_stat > crit.val.t
      test_stat1 <- ((estimate - (ATE - margin)) - margin)/std.err
      type1.err <- test_stat1 > crit.val.t

      res <- c(estimate, std.err, test_stat, power, coverage, MSE, type1.err) %>% stats::setNames(nm = c("estimate", "std.err", "test_stat", "power", "coverage", "MSE", "type1.err"))

      if (est.power) {
        if (coefs[3] == 0 & is.null(adj.covs)) {
          stop("The parameter c in the data generating process is 0 withour any other adjustment covariates, meaning Sigma_X becomes singular.")
        }

        if (!is.null(data.list[[k]]$hist_test)) {
          hist <- rbind(data.list[[k]]$hist, data.list[[k]]$hist_test)
        } else {
          hist <- data.list[[k]]$hist
        }

        hist1 <- hist %>% dplyr::mutate(w = 1)
        hist0 <- hist %>% dplyr::mutate(w = 0)

        X <- stats::model.matrix(stats::formula(paste0("y ~ 1 +", paste0("(", paste0("x", 1:N.covs, collapse = "+"), ")^2"),
                                                       "+", paste0("I(x", 1:N.covs, "^2)", collapse = "+"),
                                                       "+", paste0("I(x", 1:N.covs, "*w)", collapse = "+"))),
                                 data = hist1)
        hist$pred1 <- X %*% c(ATE, rep(coefs[2], N.covs), rep(coefs[1], N.covs), rep(coefs[3], N.covs), rep(coefs[1], ncol(X) - (3*N.covs + 1)))
        X <- stats::model.matrix(stats::formula(paste0("y ~ -1 +", paste0("(", paste0("x", 1:N.covs, collapse = "+"), ")^2"),
                                                       "+", paste0("I(x", 1:N.covs, "^2)", collapse = "+"))),
                                 data = hist0)
        hist$pred0 <- X %*% c(rep(coefs[2], N.covs), rep(coefs[1], N.covs), rep(coefs[1], ncol(X) - (2*N.covs)))


        prelim <- power.ancova(data.hist = hist, outcome.var = outcome.var, treatment.var = treatment.var,
                               adj.covs = c(adj.covs, "pred1", "pred0"), interaction = interaction, n = n, r = r, ATE = ATE, margin = margin,
                               alpha = alpha)
        res <- c(res, prelim)

      }
      res
    }


    if (parallel) {
      oplan <-  future::plan(future::multicore, workers = workers)
      on.exit(plan(oplan))
      out <- future.apply::future_lapply(X = 1:N.sim, future.seed = TRUE, FUN = oracle.sim)
    } else {
      out <- lapply(1:N.sim, FUN = oracle.sim)
    }

    out <- dplyr::bind_rows(out)
  }

  return(out)
}
