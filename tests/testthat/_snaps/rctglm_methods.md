# `estimand` method works

    Code
      est1
    Output
        Estimate Std. Error
      1 1.325113   1.152001

---

    Code
      estimand(ate_wo_cvvariance)
    Output
        Estimate Std. Error
      1 1.325113  0.9875967

# `coef` method works

    Code
      coef(ate)
    Output
      (Intercept)          X1           A 
         1.203277    1.397944    1.325113 

