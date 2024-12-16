#!/bin/bash
#SBATCH --job-name=comp-ad                  # create a name for the job
#SBATCH --nodes=1                           # Node count (number of computers)
#SBATCH --ntasks-per-node=1                 # total number of tasks across all nodes
#SBATCH --cpus-per-task=1                   # cpu-cores per task
#SBATCH --mem-per-cpu=20G                   # memory per cpu-core (4G is default)
#SBATCH --time=1:00:00                      # total run time limit (HH:MM:SS)
#SBATCH --output=HPC/Output/slurm-%A.%a.out # Output file
#SBATCH --error=HPC/Error/slurm-%A.%a.err   # Error file

echo "My SLURM_ARRAY_JOB_ID is $SLURM_ARRAY_JOB_ID."
echo "My SLURM_ARRAY_TASK_ID is $SLURM_ARRAY_TASK_ID."
echo "Executing on the machine:" $(hostname)

export OMP_NUM_THREADS=1

module load R/4.2.2

Rscript R/compile-ad-analysis.R
