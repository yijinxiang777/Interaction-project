#!/usr/bin/env bash
#SBATCH --mail-type=ALL
#SBATCH --mail-user=yijin.xiang@emory.edu
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=24
#SBATCH --time=10:00:00
#SBATCH --job-name=multi_interaction
#SBATCH --mem=120g
#SBATCH --partition=naimi
#SBATCH --array=1-16

module purge
module load R

# Natural Course
Rscript --no-save --no-restore --verbose multi_int_code_0221.R  $SLURM_ARRAY_TASK_ID testing.Rout 2>&1
