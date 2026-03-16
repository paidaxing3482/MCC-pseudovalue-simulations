#!/bin/bash
#SBATCH --account=def-jgrons
#SBATCH --time=02:00:00
#SBATCH --cpus-per-task=1
#SBATCH --mem=8G
#SBATCH --job-name=power_array
#SBATCH --output=power_array_%A_%a.out
#SBATCH --array=1-3

module load r
Rscript run_power_array.R