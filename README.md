# Replication code for *Characterizing Discrete Choice Model Tradeoffs in Prediction Accuracy, Tractability, and Substitution Patterns*

## Overview
This repository contains code to evaluate tradeoffs among discrete choice models with respect to out-of-sample predictive accuracy, substitution distortion (invariance), and computational time. 
It evaluates a set of classic models, including the multinomial (simple) logit, nested logit, latent class logit, and mixed logit, as well as exploring the less prevalent latent class nested logit.

## Requirements
- R version 4.5.1 or higher
- Platform: Windows, macOS, or Linux (tested on Windows 11 and HPC Linux cluster)
- Required packages:
  - `apollo` - Discrete choice model estimation software (and associated dependencies), version ‘0.3.6’
  - `readxl` - Reading Excel data files
  - `dplyr` - Data manipulation
  - `tidyr` - Data tidying
  - `tidyverse` - Collection of data science packages
  - `parallel` - Parallel computing for computationally intensive estimations

## Repository Structure
```
├── core/                      # Core functions for model estimation and analysis
├── hpc/                       # HPC cluster scripts and job files
├── inputs/                    # Input data and HPC batch configuration files
│   ├── hpc_main/             # Main batch run configurations and shell scripts
│   ├── MNL_pref_estimates.csv
│   └── PooledWeighted.xlsx
├── runners_archive/           # Archived debugging and development scripts
├── output/                    # Local execution outputs (gitignored)
├── logs/                      # HPC execution outputs (gitignored)
├── solutions/                 # Preserved results for version control
├── main_runner.R              # Main script to run analyses locally
├── timing_replication.R       # Script to replicate timing comparisons
├── analysis_visualizations.qmd # Quarto document for generating figures and tables
└── README.md
```

### Key Directories

**`core/`** - Core analysis functions
- Model estimation functions and Apollo wrappers
- Data loading and preprocessing utilities
- Custom cross-validation and out-of-sample prediction functions
- Simulation and speed testing utilities

**`hpc/`** - High-performance computing scripts
- SLURM job submission scripts (`.job`, `.sbatch`)
- R scripts for batch model estimation, cross-validation, and multistart
- Package installation scripts for HPC environment
- Batch configuration files

**`inputs/`** - Input data and configurations
- Survey data files (`PooledWeighted.xlsx`)
- MNL preference estimates (`MNL_pref_estimates.csv`)
- `hpc_main/` subdirectory contains HPC batch settings and shell scripts for main estimation runs

**`solutions/`** - Archived model results
- Contains preserved estimation results for reproducibility
- `main2_11-11-25/` folder contains the main results presented in the manuscript

### Key Files

- `main_runner.R` - Primary entry point for local model estimation
- `timing_replication.R` - Replicates computational timing comparisons from the paper
- `analysis_visualizations.qmd` - Generates paper figures and tables

## Usage

### Local Execution
1. Install the required packages in R (see Installation section)
2. Run `main_runner.R`, adjusting settings as defined in that file
3. For timing replication: run `timing_replication.R`, specifying the folder containing estimated model results
4. For data visualizations: run `analysis_visualizations.qmd`, specifying the folder containing estimated model results

#### Outputs (Local)
- Model results are saved to the `output/` folder
- Note: The `output/` folder is ignored by git. To preserve results in version control, copy files to `solutions/`

### High-Performance Computer Execution

#### Initial Setup
1. Copy the repository to your HPC system
2. Install required packages by submitting the installation job:
```bash
   sbatch hpc/install_r_packages.job
```
   Note: You may need to adjust the file path in `hpc/install_r_packages.R` depending on your HPC configuration

#### Running Models

**For quick test runs:**
```bash
# Submit test job
sbatch hpc/test_run.job
```

**For full estimation runs:**

Due to HPC time limits (72 hours on PSC), computationally intensive models (e.g., mixed logit with many draws) are split into separate stages:

1. Multistart estimation:
```bash
   sbatch inputs/hpc_main/hpc_run_main2b-big_ms.sh
```
   **Important:** Note the job ID returned after submission (e.g., `35774751`)

2. Update subsequent job files with the job ID from step 1:
   - Open `inputs/hpc_main/hpc_run_main2b-big_est.sh` and `inputs/hpc_main/hpc_run_main2b-big_cv.sh`
   - Update the `ORIGINAL_JOB_ID` variable with the job ID from step 1
   - Verify/adjust `mem_per_core` setting (default: 2 GB for RM-shared partition)
   
   Example:
```bash
   ORIGINAL_JOB_ID="35774751"  # Replace with job ID from step 1
   mem_per_core=2              # GB per core
```

3. Parameter estimation:
```bash
   sbatch inputs/hpc_main/hpc_run_main2b-big_est.sh
```

4. Cross-validation:
```bash
   sbatch inputs/hpc_main/hpc_run_main2b-big_cv.sh
```

#### Outputs (HPC)
- Model results and execution logs are saved to the `logs/` folder
- Note: The `logs/` folder is ignored by git. To preserve results in version control, copy files to `solutions/`

## Data

The analysis uses vehicle choice survey data from:

**Source:** Forsythe, C. R., Gillingham, K. T., Michalek, J. J., and Whitefoot, K. S., 2023, "Technology advancement is driving electric vehicle adoption," *Proceedings of the National Academy of Sciences*, 120(23)

**Repository:** https://github.com/crforsythe/DrivingElectrificationReplication

The survey data is included in this repository as:
- `inputs/PooledWeighted.xlsx` - Pooled and weighted survey responses
- `inputs/MNL_pref_estimates.csv` - Multinomial logit preference estimates

**Citation for original data:**
```
Forsythe, C. R., Gillingham, K. T., Michalek, J. J., and Whitefoot, K. S. (2023). 
Technology advancement is driving electric vehicle adoption. 
Proceedings of the National Academy of Sciences, 120(23).
```

Please cite both this repository and the original data source when using this data.


## License



