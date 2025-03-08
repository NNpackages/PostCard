# `rctglm` snapshot tests

    Code
      estimand(ate_wo_cv)
    Output
        Estimate Std. Error
      1 1.762089  0.1885315

---

    Code
      estimand(ate_with_cv)
    Output
        Estimate Std. Error
      1 1.762089  0.1932432

# `estimand_fun_derivX` can be left as NULL or specified manually

    Code
      ate_auto <- withr::with_seed(42, {
        rctglm(formula = Y ~ ., group_indicator = A, data = dat_gaus, family = gaussian,
        estimand_fun = "ate", verbose = 1)
      })
    Message
      i Setting the group allocation probability `group_allocation_prob` as the mean of column `A` in data: 0.38
      
      -- Symbolic differentiation of estimand function --
      
      i Symbolically deriving partial derivative of the function 'psi1 - psi0' with respect to 'psi0' as: '-1'.
      * Alternatively, specify the derivative through the argument
      `estimand_fun_deriv0`
      i Symbolically deriving partial derivative of the function 'psi1 - psi0' with respect to 'psi1' as: '1'.
      * Alternatively, specify the derivative through the argument
      `estimand_fun_deriv1`

