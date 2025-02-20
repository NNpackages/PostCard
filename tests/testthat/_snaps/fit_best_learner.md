# `get_best_learner` print information when verbose > 0 [plain]

    Code
      get_best_learner(resamples = cv_folds, learners = default_learners(), formula = "y ~ x1",
      verbose = 2)
    Message
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
      i Model with lowest RMSE: mod_lm
    Output
      == Workflow ====================================================================
      Preprocessor: Formula
      Model: linear_reg()
      
      -- Preprocessor ----------------------------------------------------------------
      y ~ x1
      
      -- Model -----------------------------------------------------------------------
      Linear Regression Model Specification (regression)
      
      Computational engine: lm 
      

# `get_best_learner` print information when verbose > 0 [ansi]

    Code
      get_best_learner(resamples = cv_folds, learners = default_learners(), formula = "y ~ x1",
      verbose = 2)
    Message
      [36mi[39m Fitting learners
      * mod_mars
      * mod_lm
      * mod_gbt
      [34mi[39m	[30mNo tuning parameters. `fit_resamples()` will be attempted[39m
      [34mi[39m [30m1 of 3 resampling: mod_mars[39m
      [32mv[39m [30m1 of 3 resampling: mod_mars[39m[30m ()[39m
      [34mi[39m	[30mNo tuning parameters. `fit_resamples()` will be attempted[39m
      [34mi[39m [30m2 of 3 resampling: mod_lm[39m
      [32mv[39m [30m2 of 3 resampling: mod_lm[39m[30m ()[39m
      [34mi[39m [30m3 of 3 tuning:     mod_gbt[39m
      [32mv[39m [30m3 of 3 tuning:     mod_gbt[39m[30m ()[39m
      [36mi[39m Model with lowest RMSE: mod_lm
    Output
      == Workflow ====================================================================
      [3mPreprocessor:[23m Formula
      [3mModel:[23m linear_reg()
      
      -- Preprocessor ----------------------------------------------------------------
      y ~ x1
      
      -- Model -----------------------------------------------------------------------
      Linear Regression Model Specification (regression)
      
      Computational engine: lm 
      

# `get_best_learner` print information when verbose > 0 [unicode]

    Code
      get_best_learner(resamples = cv_folds, learners = default_learners(), formula = "y ~ x1",
      verbose = 2)
    Message
      ℹ Fitting learners
      • mod_mars
      • mod_lm
      • mod_gbt
      i	No tuning parameters. `fit_resamples()` will be attempted
      i 1 of 3 resampling: mod_mars
      ✔ 1 of 3 resampling: mod_mars ()
      i	No tuning parameters. `fit_resamples()` will be attempted
      i 2 of 3 resampling: mod_lm
      ✔ 2 of 3 resampling: mod_lm ()
      i 3 of 3 tuning:     mod_gbt
      ✔ 3 of 3 tuning:     mod_gbt ()
      ℹ Model with lowest RMSE: mod_lm
    Output
      ══ Workflow ════════════════════════════════════════════════════════════════════
      Preprocessor: Formula
      Model: linear_reg()
      
      ── Preprocessor ────────────────────────────────────────────────────────────────
      y ~ x1
      
      ── Model ───────────────────────────────────────────────────────────────────────
      Linear Regression Model Specification (regression)
      
      Computational engine: lm 
      

# `get_best_learner` print information when verbose > 0 [fancy]

    Code
      get_best_learner(resamples = cv_folds, learners = default_learners(), formula = "y ~ x1",
      verbose = 2)
    Message
      [36mℹ[39m Fitting learners
      • mod_mars
      • mod_lm
      • mod_gbt
      [34mi[39m	[30mNo tuning parameters. `fit_resamples()` will be attempted[39m
      [34mi[39m [30m1 of 3 resampling: mod_mars[39m
      [32m✔[39m [30m1 of 3 resampling: mod_mars[39m[30m ()[39m
      [34mi[39m	[30mNo tuning parameters. `fit_resamples()` will be attempted[39m
      [34mi[39m [30m2 of 3 resampling: mod_lm[39m
      [32m✔[39m [30m2 of 3 resampling: mod_lm[39m[30m ()[39m
      [34mi[39m [30m3 of 3 tuning:     mod_gbt[39m
      [32m✔[39m [30m3 of 3 tuning:     mod_gbt[39m[30m ()[39m
      [36mℹ[39m Model with lowest RMSE: mod_lm
    Output
      ══ Workflow ════════════════════════════════════════════════════════════════════
      [3mPreprocessor:[23m Formula
      [3mModel:[23m linear_reg()
      
      ── Preprocessor ────────────────────────────────────────────────────────────────
      y ~ x1
      
      ── Model ───────────────────────────────────────────────────────────────────────
      Linear Regression Model Specification (regression)
      
      Computational engine: lm 
      

