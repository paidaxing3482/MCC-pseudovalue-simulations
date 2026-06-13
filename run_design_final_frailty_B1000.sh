#!/bin/bash
#SBATCH --account=def-jgrons
#SBATCH --time=36:00:00
#SBATCH --cpus-per-task=1
#SBATCH --mem=8G
#SBATCH --job-name=design_B1000
#SBATCH --output=design_B1000_%j.out
#SBATCH --error=design_B1000_%j.err

module load StdEnv/2023 r/4.5.0

export B=1000

Rscript 13_design_vignette_final_frailty.R
