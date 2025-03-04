# PostCard 0.2.1.9000 (development version)

Major overhaul of package. Still focuses on analysing data with the use of
prognostic scores, but takes a more general approach that allows any
distribution of response and covariates within the scope of generalised linear
models (GLMs) and does not necessarily run on a number of data sets created
by simulation.

The package provides novel methods for:

* `rctglm`: Finding any marginal effect estimand and estimating the standard error using
  influence functions to avoid inflation of type 1 error
* `rctglm_with_prognosticscore`: Do the above, but leveraging historical data
  to increase precision with prognostic scores.

# PostCard 0.2.1

## Features

* Added function `simulate_collection` that takes function arguments for how to
  simulate covariates and model the outcome in the historical and "current" data
  to give the user full flexibility (previously a multivariate normal
  distribution was assumed)

  * `sim.lm` which simulates data from a multivariate normal distribution and
    models the outcome with a linear model is now a wrapper of
    the new - more general - `simulate_collection`.

# PostCard 0.2.0

## Features

* Added option to use sandwich HC estimators for the covariance matrix in
  `sim.lm`
  
* Updated default value of `ATE_shift` in `sim.lm`

## Code cleanup

* Modularised code. Fx. split `lm.hist` into `lm.procova` and `lm.psm`

* Renamed some functionalities

* Correcting errors in documentation

## Best practices package

* Updated DESCRIPTION

* Created README

* Added explicit package imports in form of `foo::xx`

* Added a few tests

# PostCard 0.1.0

Initial package created from local files. Package contains functionalities to
create simulation study for a specific purpose related to an article.
Functionalities include generation of a collection of data sets and a way to
analyse these data sets assuming a special case of multivariate normal
distribution of covariates with a linear model of the response. In addition,
functionalities to estimate the power of certain parameter tests based on the
results.
