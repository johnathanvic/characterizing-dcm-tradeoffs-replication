#!/bin/bash
#SBATCH --job-name=run_big-est
#SBATCH --array=1-4 # TAG - ensure matches array from multistart
#SBATCH --partition=RM-shared 
#SBATCH --ntasks-per-node=1      # Prevent full node billing
#SBATCH --cpus-per-task=32
#SBATCH --time=72:00:00
#SBATCH --output=logs/apollo_est_%A_%a.out
#SBATCH --error=logs/apollo_est_%A_%a.err
#SBATCH --mail-type=END,FAIL

# Specify the original job ID from multistart (CHANGE THIS!)
ORIGINAL_JOB_ID="35774751"  # TAG - Replace with actual job ID from multistart run

mem_per_core=2   # GB per core (2 for RM-shared)

### INITIALIZE ###
cd $SLURM_SUBMIT_DIR
# Print slurm IDs
echo "SLURM_ARRAY_JOB_ID: $SLURM_ARRAY_JOB_ID (current estimation job)"
echo "SLURM_JOB_ID: $SLURM_JOB_ID"
echo "SLURM_ARRAY_TASK_ID: $SLURM_ARRAY_TASK_ID"
echo "ORIGINAL_JOB_ID: $ORIGINAL_JOB_ID (multistart job)"

# Verify that multistart results exist
if [ ! -d "logs/job_${ORIGINAL_JOB_ID}" ]; then
    echo "ERROR: Multistart job directory logs/job_${ORIGINAL_JOB_ID} not found!"
    exit 1
fi

# Check that the specific model's multistart results exist
MULTISTART_COMPLETE=$(find logs/job_${ORIGINAL_JOB_ID} -name "part1_multistart_results.rds" | wc -l)
if [ "$MULTISTART_COMPLETE" -eq "0" ]; then
    echo "ERROR: No multistart results found in logs/job_${ORIGINAL_JOB_ID}/"
    echo "Make sure multistart (part1) completed successfully first."
    exit 1
fi

mkdir -p ~/R/library
export R_LIBS_USER=~/R/library
export OMP_NUM_THREADS=1

### RUN ESTIMATION ###
echo "Running estimation for model $SLURM_ARRAY_TASK_ID using multistart results from job $ORIGINAL_JOB_ID..."
Rscript hpc/hpc_run_est.R $SLURM_ARRAY_TASK_ID $SLURM_CPUS_PER_TASK $mem_per_core $ORIGINAL_JOB_ID

### ORGANIZE OUTPUTS ###
# Create a completion marker for this task in the original job folder
touch logs/job_${ORIGINAL_JOB_ID}/estimation_task_${SLURM_ARRAY_TASK_ID}_complete

# Move log files to original job directory
mv logs/apollo_est_${SLURM_ARRAY_JOB_ID}_${SLURM_ARRAY_TASK_ID}.out logs/job_${ORIGINAL_JOB_ID}/ 2>/dev/null
mv logs/apollo_est_${SLURM_ARRAY_JOB_ID}_${SLURM_ARRAY_TASK_ID}.err logs/job_${ORIGINAL_JOB_ID}/ 2>/dev/null

echo "Done with estimation. Results and logs saved to logs/job_${ORIGINAL_JOB_ID}/"