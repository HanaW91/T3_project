# T3 Project: Simulation Regression Framework

This project explores simulation-based benchmarking for variable selection,
using the `fake` R package as the main simulation framework.

The current focus is to understand `fake::SimulateRegression()` and how it can
be adapted for the project aim: studying variable selection with binary or
categorical predictors generated using percentile cutoffs.

## Files

- `code/`: R scripts and analysis code.
- `results/`: generated CSV result tables.
- `plots/`: generated figures and plots.
- `code/run_fake_simulate_regression_demo.R`: direct familiarisation script using
  `fake::SimulateRegression` and `fake::SimulateGraphical`.
- `code/run_fake_percentile_adaptation.R`: prototype showing where
  percentile-based binary conversion can be inserted into the `fake` workflow
  before calling `fake::SimulateRegression(xdata = modified_X)`.
- `code/run_fake_toy_benchmark.R`: toy LASSO benchmark using fake data,
  multiple scaling methods, and a 10% active predictor setting.

## Running in Positron

Open this folder in Positron and run:

```r
source("code/run_fake_simulate_regression_demo.R")
```

This directly runs the requested `fake::SimulateRegression` framework.

To run the percentile-based adaptation prototype:

```r
source("code/run_fake_percentile_adaptation.R")
```

To run a toy LASSO performance benchmark across scaling methods:

```r
source("code/run_fake_toy_benchmark.R")
```

This writes:

- `results/fake_toy_benchmark_results.csv`: one row per seed, scaling method,
  and toy data setting.
- `results/fake_toy_benchmark_summary.csv`: average LASSO performance for each
  scaling combination.

The benchmark records prediction metrics (`rmse`, `mae`, `r_squared`) and
variable-selection metrics (`precision`, `recall`, `f1_score`, `sensitivity`,
`false_discovery_rate`, and `specificity`).

The toy benchmark starts with `n = 1000`, `pk = 100`, and `nu_xy = 0.10`,
matching the first baseline setting suggested for getting a feel for
performance.

The scaling methods are:

- `none`: no manual scaling.
- `zscore`: subtract the mean and divide by one standard deviation.
- `2sd`: subtract the mean and divide by two standard deviations.

The scripts use `fake` for the package demonstration and `glmnet` for LASSO
regression. If needed, install them with:

```r
install.packages(c("fake", "glmnet"))
```

## Current Research Direction

The planned workflow is:

1. Use `fake::SimulateGraphical()` to generate correlated continuous predictors.
2. Convert selected predictors to binary/categorical form using percentile
   cutoffs, for example 50/50 or 80/20 binary splits.
3. Use `fake::SimulateRegression(xdata = modified_X)` to generate outcomes and
   ground truth (`theta`, `beta`).
4. Fit variable selection methods and compare selected predictors with the known
   active predictors.

The current code is a familiarisation/prototype step, not the final simulation
study.
