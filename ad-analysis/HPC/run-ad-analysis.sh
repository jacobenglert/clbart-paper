#!/bin/bash
#SBATCH --job-name=ad-analysis
#SBATCH --nodes=1                           # Node count (number of computers)
#SBATCH --ntasks-per-node=1                 # total number of tasks across all nodes
#SBATCH --cpus-per-task=1                   # cpu-cores per task (>1 if multi-threaded tasks, internal parallelization within R for example)
#SBATCH --mem-per-cpu=10G                   # memory per cpu-core (4G is default)
#SBATCH --array=1-180          	            # Job array with index values. Show up as SLURM_ARRAY_TASK_ID
#SBATCH --time=30-00:00:00
#SBATCH --output=HPC/Output/slurm-%A.%a.out # Output file
#SBATCH --error=HPC/Error/slurm-%A.%a.err   # Error file

echo "My SLURM_ARRAY_JOB_ID is $SLURM_ARRAY_JOB_ID."
echo "My SLURM_ARRAY_TASK_ID is $SLURM_ARRAY_TASK_ID."
echo "Executing on the machine:" $(hostname)

export OMP_NUM_THREADS=1

module load R/4.2.2

Rscript R/run-ad-analysis.R $SLURM_ARRAY_TASK_ID
