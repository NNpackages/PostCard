
<!-- README.md is generated from README.Rmd. Please edit that file -->

# PostCard

<!-- badges: start -->

[![R-CMD-check](https://github.com/NNpackages/PostCard/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/NNpackages/PostCard/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/NNpackages/PostCard/graph/badge.svg)](https://app.codecov.io/gh/NNpackages/PostCard)
<!-- badges: end -->

PostCard is a package for PrOgnoSTic CovARiate aDjustment in randomised
clinical trials. At the time of writing, the package features convenient
functions for conducting analysis using prognostic covariate adjustment
for GLMs. Let $\Psi_a=\E[Y(a)]$ be the population mean outcome under
treatment $a=0, 1$, sometimes referred to as *counterfactual means*. We
are interested in marginal effects, which are causal effects of the form
$r(\Psi_1, \Psi_0)$. The package uses plug-in estimation for robust
estimation of a marginal effect estimand and (CV) influence functions
for robust estimation of the variance of the estimand (Rosenblum, M. and
M. J. van der Laan, 2010: Simple, efficient estimators of treatment
effects in randomized trials using generalized linear models to leverage
baseline variables. The International Journal of Biostatistics, 6, no.
1).

## Installation

You can install the development version of PostCard from
[GitHub](https://github.com/) with:

``` r
pak::pak("NNpackages/PostCard")
```

Setup-chunk to load the package, set a seed and turn off verbosity for
the rendering of the README.

``` r
library(PostCard)
withr::local_seed(1395878)
withr::local_options(list(PostCard.verbose = 0))
```

## Simulating data for exploratory analyses

First, we simulate some data to be able to enable showcasing of the
functionalities. For this we use the `glm_data()` function from the
package, where the user can specify an expression alongside variables
and a family of the response to then simulate a response from a GLM with
linear predictor given by the expression provided.

``` r
n <- 1000
b0 <- 1
b1 <- 3
b2 <- 2

# Simulate data with a non-linear effect
dat_treat <- glm_data(
  Y ~ b0+b1*sin(W)^2+b2*A,
  W = runif(n, min = -2, max = 2),
  A = rbinom(n, 1, prob = 1/2),
  family = gaussian() # Default value
)
```

## Plug-in estimation of marginal effects and variance estimation using influence functions

### Fitting `rctglm()` without prognostic covariate adjustment

The `rctglm()` function estimates any specified estimand using plug-in
estimation for randomised clinical trials and estimates the variance
using the influence function of the marginal effect estimand.

The interface of `rctglm()` is similar to that of the `stats::glm()`
function but with an added mandatory specification of

- The randomisation variable in data, usually being the (name of) the
  treatment variable
- The randomisation ratio - the probability of being allocated to group
  1 (rather than 0)
  - As a default, a ratio of 1’s in data is used
- An estimand function
  - As a default, the function takes the average treatment effect (ATE)
    as the estimand

Thus, we can estimate the ATE by simply writing the below:

> Note that as a default, information about the algorithm is printed in
> the console, but here we suppress this behavior. See more in
> `vignette("more-details")`.

``` r
ate <- rctglm(formula = Y ~ A * W,
              exposure_indicator = A,
              exposure_prob = 1/2,
              data = dat_treat,
              family = "gaussian") # Default value
```

This creates an `rctglm` object which prints as

``` r
ate
#> 
#> Object of class rctglm 
#> 
#> Call:  rctglm(formula = Y ~ A * W, exposure_indicator = A, exposure_prob = 1/2, 
#>     family = "gaussian", data = dat_treat)
#> 
#> Counterfactual control mean (psi_0=E[Y|X, A=0]) estimate: 2.776
#> Counterfactual control mean (psi_1=E[Y|X, A=1]) estimate: 4.867
#> Estimand function r: psi1 - psi0
#> Estimand (r(psi_1, psi_0)) estimate (SE): 2.091 (0.09229)
```

### Structure of `rctglm` and methods for extracting entities

The `print` method for the `rctglm` class specifies the way to print
such an object as seen above. Behind the S3 class of `rctglm` it is a
list with

- Estimand related information
  - A `data.frame` of plug-in estimate of estimand, standard error (SE)
    estimate and variance estimate of estimand
    - Accessible through `ate$estimand` or using methods `estimand(ate)`
      and `est(ate)`
  - Counterfactual predictions (for each observation) and counterfactual
    means for both groups
    - Accessible through `ate$counterfactual_mean<0/1>` and
      `ate$counterfactual_pred<0/1>`
- Information on the underlying `glm` fit
  - Entire `glm` object available through `ate$glm`
  - Method `coef` uses the corresponding method on the `glm` object
    contained within `rctglm`

Thus, methods available are:

``` r
# "estimate" also available as alternative to just "est"
est(ate)
#>   Estimate Std. Error
#> 1 2.091095 0.09229488
coef(ate)
#> (Intercept)           A           W         A:W 
#>  2.77585401  2.09122279  0.02364106  0.04961895
```

See more info in the documentation page `rctglm_methods()`.

### Using prognostic covariate adjustment

The `rctglm_with_prognosticscore()` function uses the
`fit_best_learner()` function to fit a prognostic model to historical
data and then uses the prognostic model to predict

for all observations in the current data set. These *prognostic scores*
are then used as a covariate in the GLM when running `rctglm()`.

Allowing the use of complex non-linear models to create such a
prognostic score allows utilising information from potentially many
variables, “catching” non-linear relationships and then using all this
information in the GLM model using a single covariate adjustment.

We simulate some historical data to showcase the use of this function as
well:

``` r
dat_notreat <- glm_data(
  Y ~ b0+b1*sin(W)^2,
  W = runif(n, min = -2, max = 2),
  family = gaussian # Default value
)
```

The call to `rctglm_with_prognosticscore()` is the same as to `rctglm()`
but with an added specification of

- (Historical) data to fit the prognostic model using
  `fit_best_learner()`
- A formula used when fitting the prognostic model
  - Default uses all covariates in the data.
- (Optionally) number folds in cross validation and a list of learners
  for fitting the best learner

Thus, a simple call which estimates the average treatment effect,
adjusting for a prognostic score, is seen below:

``` r
ate_prog <- rctglm_with_prognosticscore(
  formula = Y ~ A * W,
  exposure_indicator = A,
  exposure_prob = 1/2,
  data = dat_treat,
  family = gaussian(link = "identity"), # Default value
  data_hist = dat_notreat)
```

Quick results of the fit can be seen by printing the object:

``` r
ate_prog
#> 
#> Object of class rctglm_prog 
#> 
#> Call:  rctglm_with_prognosticscore(formula = Y ~ A * W, family = gaussian(link = "identity"), 
#>     data = dat_treat, exposure_indicator = A, exposure_prob = 1/2, 
#>     data_hist = dat_notreat)
#> 
#> Counterfactual control mean (psi_0=E[Y|X, A=0]) estimate: 2.824
#> Counterfactual control mean (psi_1=E[Y|X, A=1]) estimate: 4.822
#> Estimand function r: psi1 - psi0
#> Estimand (r(psi_1, psi_0)) estimate (SE): 1.999 (0.06393)
```

It’s evident that in this case where there is a non-linear relationship
between the covariate we observe and the response, adjusting for the
prognostic score reduces the standard error of our estimand
approximation by quite a bit.

#### Investigating the prognostic model

Information on the prognostic model is available in the list element
`prognostic_info` of the resulting object. This contains

- The fitted prognostic model as a `workflow`
  - Accessible through `ate$prognostic_info$model_fit` or with method
    `prog_model(ate)`
- A list of the learners used for fitting the model using
  `fit_best_learner()`
  - Accessible through `ate$prognostic_info$learners`
- The number of folds used for cross validation (`cv_prog_folds`) and
  the historical data used for fitting the model
