# `rctglm_with_prognosticscore` snapshot tests

    Code
      ate <- withr::with_seed(42, {
        rctglm_with_prognosticscore(formula = Y ~ ., group_indicator = A, data = dat_treat,
        family = gaussian(), estimand_fun = "ate", data_hist = dat_notreat, verbose = 2)
      })
    Message
      
      -- Fitting prognostic model --
      
      i Created formula for fitting prognostic model as: Y ~ .
      i Fitting learners
      * mod_mars
      * mod_lm
      * mod_gbt
      i	No tuning parameters. `fit_resamples()` will be attempted
      i 1 of 3 resampling: mod_mars
      v 1 of 3 resampling: mod_mars ()
      i	No tuning parameters. `fit_resamples()` will be attempted
      i 2 of 3 resampling: mod_lm
      v 2 of 3 resampling: mod_lm ()
      i 3 of 3 tuning:     mod_gbt
      v 3 of 3 tuning:     mod_gbt ()
      i Model with lowest RMSE: mod_gbt
      i Investigate trained learners and fitted model in `prognostic_info` list element
      i Setting the group allocation probability `group_allocation_prob` as the mean of column `A` in data: 0.56
      
      -- Symbolic differentiation of estimand function --
      
      i Symbolically deriving partial derivative of the function 'psi1 - psi0' with respect to 'psi0' as: '-1'.
      * Alternatively, specify the derivative through the argument
      `estimand_fun_deriv0`
      i Symbolically deriving partial derivative of the function 'psi1 - psi0' with respect to 'psi1' as: '1'.
      * Alternatively, specify the derivative through the argument
      `estimand_fun_deriv1`

---

    Code
      ate_wo_cvvariance
    Output
      
      Object of class rctglm_prog 
      
      Call:  rctglm_with_prognosticscore(formula = Y ~ ., family = gaussian(), 
          data = dat_treat, group_indicator = A, estimand_fun = "ate", 
          cv_variance = FALSE, data_hist = dat_notreat, verbose = 0)
      
      Counterfactual control mean (psi_0=E[Y|X, A=0]) estimate: 2.017
      Counterfactual control mean (psi_1=E[Y|X, A=1]) estimate: 3.975
      Estimand function r: psi1 - psi0
      Estimand (r(psi_1, psi_0)) estimate (SE): 1.957 (0.2076)

---

    Code
      ate_pois
    Output
      
      Object of class rctglm_prog 
      
      Call:  rctglm_with_prognosticscore(formula = Y ~ ., family = poisson(), 
          data = dat_treat_pois, group_indicator = A, estimand_fun = "rate_ratio", 
          data_hist = dat_notreat_pois, verbose = 0)
      
      Counterfactual control mean (psi_0=E[Y|X, A=0]) estimate: 8.609
      Counterfactual control mean (psi_1=E[Y|X, A=1]) estimate: 64.14
      Estimand function r: psi1/psi0
      Estimand (r(psi_1, psi_0)) estimate (SE): 7.45 (0.456)

