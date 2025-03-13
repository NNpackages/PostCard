#' Guenther-Schouten Power Approximation
#'
#' @description
#' This function calculates the Guenther-Schouten power approximation for ANOVA or ANCOVA.
#' The approximation is based in (Guenther WC. Sample Size Formulas for Normal Theory T Tests.
#' The American Statistician. 1981;35(4):243–244) and (Schouten HJA. Sample size formula with
#' a continuous outcome for unequal group sizes and unequal variances. Statistics in Medicine.
#' 1999;18(1):87–91).
#'
#' @param n           Number of participants in total. From this number of participants in the treatment group is \eqn{n1=(r/(1+r))n} and the control group is \eqn{n1=(1/(1+r))n}.
#' @param r           Allocation ratio \eqn{r=n1/n0}. For one-to-one randomisation r=1.
#' @param sigma       Standard deviation of \eqn{Y(w)}, where w is the treatment indicator, and we assume homoskedasticity.
#' @param rho         Correlation between outcome and adjustment covariate in univariable covariate adjustment.
#' @param R2          The estimated pooled multiple correlation coefficient between outcome and covariates.
#' @param ate         Minimum effect size that we should be able to detect.
#' @param margin      Superiority margin (for non-inferiority margin, a negative value can be provided).
#' @param alpha       Significance level. Due to regulatory guidelines when using a one-sided test, half the specified significance level is used. Thus, for standard alpha = .05, a significance level of 0.025 is used.
#'
#' @details
#' The formula in the case of an ANCOVA model with multiple covariate adjustement is:
#'
#' \deqn{
#' n=\frac{(1+r)^2}{r}\frac{(z_{1-\alpha/2}+z_{1-\beta})^2\sigma^2(1-R^2)}{(\beta_1-\beta_0-\Delta_s)^2}+\frac{(z_{1-\alpha/2})^2}{2}
#' }
#'
#' where \eqn{\widehat{R}^2\coloneqq \frac{\widehat{\sigma}_{XY}^\top \widehat{\Sigma}_X^{-1}\widehat{\sigma}_{XY}}{\widehat{\sigma}^2}},
#' we denote by \eqn{\sigma^2} the variance of the outcome, \eqn{\Sigma_X} the covariance matrix of the
#' covariates, and \eqn{\sigma_{XY}} a \eqn{p}-dimensional column vector consisting of the covariance
#' between the outcome variable and each covariate. In the univariate case \eqn{R^2} is replaced by \eqn{\rho^2}
#'
#' As a default, both `R2` and `rho` are `NULL`, meaning that power is approximated in the case
#' of an ANOVA with no adjustment covariates are present besides the binary group indicator. In
#' this case, the formula above applies but where \eqn{R^2} is replaced with \eqn{0}.
#'
#' @return
#' a `numeric` with power approximation based on the Guenther-Schouten approximation
#' @export
#'
#' @examples
#' # Approximate the power for an ANCOVA with a single adjustment covariate
#' power_gs(rho = 0.7)
#'
#' #' # Approximate power for an ANCOVA with several adjustment covariates
#' power_gs(R2 = 0.8)
#'
#' # Approximate the power for an ANOVA with 2:1 randomisation, an assumed
#' # variance of Y(w) of 4, an assumed effect size of 1.5 and a margin of 1
#' power_gs(n = 400, r = 2/3, sigma = sqrt(4), ate = 1.5, margin = 1)
#'
power_gs <- function(n = 100,
                     r = 1,
                     sigma = sqrt(2),
                     rho = NULL,
                     R2 = NULL,
                     ate = 0.6,
                     margin = 0,
                     alpha = 0.05) {

  var <- sigma^2

  no_covariate_correlation <- is.null(rho) & is.null(R2)
  if (!no_covariate_correlation) {
    error_if_both_rho_R2_given(rho, R2)
    var <- var_update_rho_R2(var, rho, R2)
  }

  power <- stats::pnorm(
    sqrt(
      r/(1 + r)^2 *
        (ate - margin)^2/var *
        (n - stats::qnorm(1 - alpha/2)^2/2)
    ) - stats::qnorm(1 - alpha/2)
  )
  return(power)
}


#' Power Approximation based on non-centrality parameter
#'
#' @description
#' This function calculates the power for ANOVA or ANCOVA based on the non-centrality parameter and the exact t-distributions.
#'
#' @inheritParams power_gs
#' @param n.adj Number of adjustment covariates. Used for calculating the degrees of freedom.
#' Specification only necessary when specifying an `R2`.
#'
#' @details
#' The prospective power estimations are based on (Kieser M. Methods and Applications of Sample Size Calculation and Recalculation in Clinical Trials. Springer; 2020).
#' The ANOVA power is calculated based on the non-centrality parameter given as
#'
#' \deqn{nc =\sqrt{\frac{r}{(1+r)^2}\cdot n}\cdot\frac{ate-margin}{\sigma},}
#'
#' where we denote by \eqn{\sigma^2} the variance of the outcome, such that the power can be estimated as
#'
#' \deqn{1-\beta = 1 - F_{t,n-2,nc}\left(F_{t, n-2, 0}^{-1}(1-\alpha/2)\right).}
#'
#' The power of ANCOVA with univariate covariate adjustment and no interaction is calculated based on the non-centrality parameter given as
#'
#' \deqn{nc =\sqrt{\frac{rn}{(1+r)^2}}\frac{ate-margin}{\sigma\sqrt{1-\rho^2}},}
#'
#' such that the power can be estimated as
#'
#' \deqn{1-\beta = 1 - F_{t,n-3,nc}\left(F_{t, n-3, 0}^{-1}(1-\alpha/2)\right).}
#'
#' The power of ANCOVA with either univariate covariate adjustment and interaction or multiple covariate adjustement with or without interaction is calculated based on the non-centrality parameter given as
#'
#' \deqn{nc =\frac{ate-margin}{\sqrt{\left(\frac{1}{n_1}+\frac{1}{n_0} + X_d^\top\left((n-2)\Sigma_X\right)^{-1}X_d \right)\sigma^2\left(1-\frac{\sigma_{XY}^\top \Sigma_X^{-1}\sigma_{XY}}{\sigma^2}\right)}}.}
#'
#' where \eqn{X_d \coloneqq \left(\overline{X}_1^1-\overline{X}_0^1, \ldots, \overline{X}_1^p-\overline{X}_0^p\right)^\top}, \eqn{\widehat{R}^2\coloneqq \frac{\widehat{\sigma}_{XY}^\top \widehat{\Sigma}_X^{-1}\widehat{\sigma}_{XY}}{\widehat{\sigma}^2}},
#' \eqn{\Sigma_X} the covariance matrix of the covariates, and \eqn{\sigma_{XY}} a \eqn{p}-dimensional column vector consisting of the covariance
#' between the outcome variable and each covariate. Since we are in the case of randomized trials the expected difference between the covariate
#' values between the to groups is 0. Furthermore, the elements of \eqn{\Sigma_X^{-1}} will be small, unless the variances are close to 0, or the covariates exhibit strong linear dependencies, so that the correlations are close to 1.
#' These scenarios are excluded since they could lead to potentially serious problems regarding inference either way. These arguments are used by Zimmermann et. al
#' (Zimmermann G, Kieser M, Bathke AC. Sample Size Calculation and Blinded Recalculation for Analysis of Covariance Models with Multiple Random Covariates. Journal of Biopharmaceutical Statistics. 2020;30(1):143–159.) to approximate
#' the non-centrality parameter as in the univariate case where \eqn{\rho^2} is replaced by \eqn{R^2}.
#'
#' Then the power for ANCOVA with `n.adj` adjustment covariates can be estimated as
#'
#' \deqn{1-\beta = 1 - F_{t,n - 2 - n.adj,nc}\left(F_{t, n - 2 - n.adj,0), 0}^{-1}(1-\alpha/2)\right).}
#'
#' @return
#' The function returns a power approximation based on the non-centrality parameter and the exact t-distribution.
#' @export
#'
#' @examples
#' # Approximate the power for an ANCOVA with a single adjustment covariate
#' power_nc(rho = 0.7)
#'
#' #' # Approximate power for an ANCOVA with several adjustment covariates
#' power_nc(R2 = 0.8, n.adj = 3)
#'
#' # Approximate the power for an ANOVA with 2:1 randomisation, an assumed
#' # variance of Y(w) of 4, an assumed effect size of 3 and a margin of 1
#' power_nc(n = 400, r = 2/3, sigma = sqrt(4), ate = 1.5, margin = 1)
#'
power_nc <- function(n = 100,
                     r = 1,
                     sigma = sqrt(2),
                     rho = NULL,
                     R2 = NULL,
                     n.adj = ifelse(is.null(rho), 0, 1),
                     ate = 0.6,
                     margin = 0,
                     alpha = 0.05){
  var <- sigma^2

  no_covariate_correlation <- is.null(rho) & is.null(R2)
  if (!no_covariate_correlation) {
    error_if_both_rho_R2_given(rho, R2)

    if (!is.null(R2) && n.adj < 2)
      cli::cli_abort(
        "When specifying {.arg R2}, this indicates a model with several
        adjustment covariates. However, {.arg n.adj} is specified as {n.adj}.
        Adjust the specification of {.arg n.adj} to match the number of
        adjustment covariates in your model or specify {.arg rho} instead if
        your model only has a single adjustment covariate.")

    var <- var_update_rho_R2(var, rho, R2)
  }

  nc <- sqrt(r/(1 + r)^2*(n)) * (ate - margin)/sqrt(var)
  df <- n - 2 - n.adj
  power <- 1 - stats::pt(q = stats::qt(1 - alpha/2, n - 2), df = df, ncp = nc)
  return(power)
}

error_if_both_rho_R2_given <- function(rho, R2) {
  if (!is.null(rho) & !is.null(R2))
    cli::cli_abort("Specify either {.arg rho} or {.arg R2}. If you adjust by multiple covariates use {.arg R2} otherwise use {.arg rho}")
  return(invisible())
}

var_update_rho_R2 <- function(var, rho, R2) {
  if (!is.null(rho) & is.null(R2))
    new_var <- var*(1 - rho^2)
  if (is.null(rho) & !is.null(R2))
    new_var <- var*(1 - R2)
  return(new_var)
}

#' Title
#'
#' @param data the data used to estimate the variance bound
#' @param response the response variable in the data for which
#' @param ...
#' @param inflation
#' @param deflation
#'
#' @details
#' See article Zimmermann, G., Kieser, M., & Bathke, A. C. (2019). Sample size
#' calculation and blinded recalculation for analysis of covariance models with
#' multiple random covariates. Journal of Biopharmaceutical Statistics, 30(1),
#' 143–159. https://doi.org/10.1080/10543406.2019.1632871
#'
#' @returns
#' @export
#'
#' @examples
#' # Generate a negative binomial response
#' nb <- glm_data(1+2*x1-x2,
#'                 x1 = rnorm(10),
#'                 x2 = rgamma(10, shape = 2),
#'                 family = MASS::negative.binomial(2))
#' variance_bound_gs(nb, response_name = "Y")
variance_bound_gs <- function(formula, data, inflation = 1.25, deflation = 1) {
  if(missing(data)) data <- environment(formula)
  mf <- match.call(expand.dots = FALSE)
  m <- match(c("formula", "data", "subset", "weights", "na.action",
               "etastart", "mustart", "offset"), names(mf), 0L)
  mf <- mf[c(1L, m)]
  mf$drop.unused.levels <- TRUE
  ## need stats:: for non-standard evaluation
  mf[[1L]] <- quote(stats::model.frame)
  mf <- eval(mf, parent.frame())

  Y <- model.response(mf)
  var_Y <- Y %>%
    var()
  var_Y_inf <- var_Y * inflation

  mt <- attr(mf, "terms")
  X <- model.matrix(mt, mf)[, -1]

  # covs <- rlang::enquos(...)
  # lapply(covs, rlang::as_name)
  # no_covs_specified <- length(covs) == 0
  # if (no_covs_specified)
  #   data_sel <- data
  # else
  #   data_sel <- data %>%
  #   dplyr::select(...)
  #
  # data_covs <- data_sel %>%
  #   dplyr::select(-tidyselect::any_of(response_name))

  Sigma_X.I <- X %>%
    cov() %>%
    chol() %>%
    chol2inv()
  sigma_XY <- cov(X, Y)

  R2 <- as.numeric((t(sigma_XY) %*% Sigma_X.I %*% sigma_XY) / var_Y_inf)
  R2_def <- R2 * deflation

  var_bound <- var_Y_inf * (1 - R2_def)
  return(var_bound)
}
