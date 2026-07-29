# Driver for full subgroup benchmark plots.
# Output:
#   full subgroup grids in plots/subgroup
#   full rare/non-rare/continuous grids in plots/subgroup/full
#   subgroup F1-only grids in plots/main_f1
#   subgroup recall/precision-only grids in plots/appendix_recall_precision.

source(file.path("code", "plotting_functions.R"))

run_subgroup_plots()
