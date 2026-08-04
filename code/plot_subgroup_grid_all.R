# Driver for full subgroup benchmark plots.
# Output:
#   main mixed rare/non-rare/continuous grids in plots/main/subgroup
#   other full subgroup grids in plots/appendix/subgroup
#   subgroup F1-only grids in plots/appendix/subgroup_f1
#   subgroup recall/precision-only grids in plots/appendix/subgroup_recall_precision

source(file.path("code", "plotting_functions.R"))

run_subgroup_plots()
