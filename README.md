# T3 Project

Simulation work for looking at category prevalence, scaling, and variable
selection with `fake` and LASSO-type methods.

The current code is still a toy/simulation setup, but the workflow now follows
the main structure I want to use:

1. Generate predictors with `fake::SimulateGraphical()`.
2. Convert selected predictors into binary variables using percentile cutoffs.
3. Generate the outcome with `fake::SimulateRegression(xdata = modified_X)`.
4. Fit LASSO models and compare selected predictors with the known true active
   predictors.

## Folders

- `code/`: R scripts.
- `results/`: CSV outputs from benchmark runs.
- `plots/`: generated figures.
- `meeting slides/`: slides used for weekly meetings.

Generated CSV files and plots are not the main source code, so they do not all
need to be committed every time.

## Main Scripts

Run scripts from the project root:

```r
setwd("C:/Users/user/OneDrive/Master/T3_project")
```

Scaling benchmark:

```r
source("code/run_scaling_benchmark.R")
source("code/plot_scaling_benchmark_results.R")
```

Noise / predictor-mix check:

```r
source("code/run_noise_benchmark.R")
source("code/plot_noise_benchmark_results.R")
```

Correlation-focused scaling plot:

```r
source("code/run_correlation_benchmark.R")
source("code/plot_scaling_by_correlation.R")
```

Imbalance benchmark:

```r
source("code/run_imbalance_benchmark.R")
source("code/plot_imbalance_benchmark_results.R")
```

Binary creation sanity check:

```r
source("code/sanity_check.R")
```

## Current Toy Setup

Baseline size:

- `n = 1000`
- `pk = 100`
- `nu_xy = 0.10`, so around 10 true active predictors
- 10 seeds for current exploratory runs

Main settings currently varied:

- `ev_xy`: signal strength / outcome explained variance
- `ev_xx`: predictor correlation
- `binary_top_fraction`: rare-category split for imbalanced binary predictors
- `pk_imbalance_fraction`: proportion of binary predictors given the rare split
- scaling: `none`, `zscore`, `2sd`

The `2sd` scaling follows the literature setup I am using here: it scales
continuous predictors only, while binary predictors stay coded as `0/1`.

## Model Outputs

The current LASSO runs use `glmnet::cv.glmnet()` with manual scaling applied
before fitting, so `standardize = FALSE`.

For each CV fit, the scripts currently record:

- `cv_lasso_min`, using `lambda.min`
- `cv_lasso_1se`, using `lambda.1se`

Main variable-selection metrics:

- precision
- recall
- F1 score
- selected variables
- true positives / false positives / false negatives

Prediction metrics are also kept, including RMSE and R-squared.

## Notes

The focused scripts are easier to use than putting every scenario in one big
file:

- scaling analysis
- noise / predictor-mix check
- correlation split
- imbalance analysis

This keeps each result plot tied to one question, which makes the weekly
meeting slides easier to explain.
