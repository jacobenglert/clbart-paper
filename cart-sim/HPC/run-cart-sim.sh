#!/bin/bash
#SBATCH --job-name=cart-sim                 # create a name for the job
#SBATCH --nodes=1                           # Node count (number of computers)
#SBATCH --ntasks-per-node=1                 # total number of tasks across all nodes
#SBATCH --cpus-per-task=1                   # cpu-cores per task
#SBATCH --mem-per-cpu=5G                    # memory per cpu-core (4G is default)
#SBATCH --time=3-00:00:00                   # total run time limit (HH:MM:SS)
#SBATCH --array=1-200                       # Job array with index values. Show up as SLURM_ARRAY_TASK_ID
#SBATCH --output=HPC/Output/slurm-%A.%a.out # Output file
#SBATCH --error=HPC/Error/slurm-%A.%a.err   # Error file

echo "My SLURM_ARRAY_JOB_ID is $SLURM_ARRAY_JOB_ID."
echo "My SLURM_ARRAY_TASK_ID is $SLURM_ARRAY_TASK_ID."
echo "Executing on the machine:" $(hostname)

export OMP_NUM_THREADS=1

key=$1
seed=$SLURM_ARRAY_TASK_ID

module load R/4.2.2

Rscript R/run-cart-sim.R $key $seed
