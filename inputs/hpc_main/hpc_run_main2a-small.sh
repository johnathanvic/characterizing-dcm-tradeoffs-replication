#!/bin/bash
#SBATCH --job-name=run_small
#SBATCH --array=1-15
#SBATCH --partition=RM-shared 
#SBATCH --ntasks-per-node=1      # Prevent full node billing
#SBATCH --cpus-per-task=32        # Two threads for task (64 max)
#SBATCH --time=72:00:00
#SBATCH --output=logs/apollo_%A_%a.out
#SBATCH --error=logs/apollo_%A_%a.err
#SBATCH --mail-type=END,FAIL

# Specify
RUN_SETTINGS="inputs/hpc_main/hpc_batch_settings_main2.R" 
MODEL_BATCH_LIST="inputs/hpc_main/hpc_models_list_batch_main2a-small.R" # ENSURE LIST LENGTH MATCHES ARRAY
mem_per_core=2   # GB per core (2 for RM-shared)

### INITIALIZE ###
cd $SLURM_SUBMIT_DIR
# Print slurm IDs
echo "SLURM_ARRAY_JOB_ID: $SLURM_ARRAY_JOB_ID"
echo "SLURM_JOB_ID: $SLURM_JOB_ID"
echo "SLURM_ARRAY_TASK_ID: $SLURM_ARRAY_TASK_ID"

# Use SLURM_ARRAY_JOB_ID to match the filename pattern
mkdir -p logs/job_${SLURM_ARRAY_JOB_ID}
echo "Created job directory: logs/job_${SLURM_ARRAY_JOB_ID}"

mkdir -p ~/R/library
export R_LIBS_USER=~/R/library
export OMP_NUM_THREADS=1

# Only run initialization for the first task
if [ "$SLURM_ARRAY_TASK_ID" = "1" ]; then
    echo "Task 1: Starting hpc/hpc_init_preparation.R..."
    Rscript hpc/hpc_init_preparation.R "$RUN_SETTINGS" "$MODEL_BATCH_LIST"
    
    # Create a flag file to indicate initialization is complete
    touch logs/job_${SLURM_ARRAY_JOB_ID}/init_complete
    echo "Task 1: Initialization complete."
else
    # Wait for initialization to complete
    echo "Task $SLURM_ARRAY_TASK_ID: Waiting for initialization to complete..."
    
    while [ ! -f logs/job_${SLURM_ARRAY_JOB_ID}/init_complete ]; do
        sleep 10
    done
    echo "Task $SLURM_ARRAY_TASK_ID: Initialization complete, proceeding..."
fi

### RUN MODELS ###
echo "Running model $SLURM_ARRAY_TASK_ID..."
Rscript hpc/hpc_run_model.R $SLURM_ARRAY_TASK_ID $SLURM_CPUS_PER_TASK $mem_per_core

### ORGANIZE OUTPUTS ###
# Create a completion marker for this task
touch logs/job_${SLURM_ARRAY_JOB_ID}/task_${SLURM_ARRAY_TASK_ID}_complete

# Check if this is the last task to complete
EXPECTED_TASKS=$(echo $SLURM_ARRAY_TASK_COUNT)
COMPLETED_TASKS=$(ls logs/job_${SLURM_ARRAY_JOB_ID}/task_*_complete 2>/dev/null | wc -l)

if [ "$COMPLETED_TASKS" -eq "$EXPECTED_TASKS" ]; then
    echo "All tasks completed - aggregating results..."
    Rscript hpc/hpc_aggregate_summaries.R "logs/job_${SLURM_ARRAY_JOB_ID}"
    
    # Clean up completion markers
    rm logs/job_${SLURM_ARRAY_JOB_ID}/task_*_complete
fi

# Move log files to job-specific directory
mv logs/apollo_${SLURM_ARRAY_JOB_ID}_${SLURM_ARRAY_TASK_ID}.out logs/job_${SLURM_ARRAY_JOB_ID}/ 2>/dev/null
mv logs/apollo_${SLURM_ARRAY_JOB_ID}_${SLURM_ARRAY_TASK_ID}.err logs/job_${SLURM_ARRAY_JOB_ID}/ 2>/dev/null

echo "Done Running. Log files moved to logs/job_${SLURM_ARRAY_JOB_ID}/"