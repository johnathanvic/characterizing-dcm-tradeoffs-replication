#!/bin/bash
#SBATCH --job-name=apollo_models
#SBATCH --array=1               # Adjust to length of all_model_specs
#SBATCH --cpus-per-task=2
#SBATCH --time=12:00:00
#SBATCH --mem=32G
#SBATCH --output=logs/apollo_%A_%a.out
#SBATCH --error=logs/apollo_%A_%a.err

module load R
export OMP_NUM_THREADS=1

# (Re-)prepare models - fast if cached, idempotent
Rscript hpc/hpc_init_preparation.R

# Run exactly one model per array ID
Rscript hpc/hpc_run_model.R $SLURM_ARRAY_TASK_ID $SLURM_CPUS_PER_TASK
