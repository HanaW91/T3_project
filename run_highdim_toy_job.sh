#!/bin/bash
#PBS -N highdim_toy
#PBS -l walltime=06:00:00
#PBS -l select=1:ncpus=8:mem=64gb
#PBS -j oe
#PBS -o highdim_toy.log

set -x

module load tools/prod
module load R/4.2.1-foss-2022a

cd "$PBS_O_WORKDIR" || exit 1

echo "Job started at: $(date)"
echo "Working directory: $(pwd)"
echo "Checking script exists:"
ls -lh code/run_highdim_toy.R

echo "Rscript location:"
which Rscript

echo "Rscript version:"
Rscript --version

echo "Starting R script..."
Rscript code/run_highdim_toy.R

echo "Job finished at: $(date)"
echo "Results folder:"
ls -lh results
