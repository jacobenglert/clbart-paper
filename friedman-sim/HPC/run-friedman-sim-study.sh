#!/bin/bash

# Clean up error and output directories
rm HPC/Error/*.err
rm HPC/Output/*.out
rm Results/temp/*.rds

# Initialize empty variable to hold all job ids
job_ids=""

# Loop through each key
for key in {1..30}
do

  # Submit the SLURM job array for each key and capture the job id
  job_id=$(sbatch HPC/run-friedman-sim.sh $key | awk '{print $4}')
  
  # Append each job id to the job_ids list
  job_ids+="$job_id:"
  
done

# Remove the trailing colon from the job_ids string
job_ids=${job_ids%:}

# Summarize simulation results after all keys have been processed
echo "Summarizing simulation results after jobs $job_ids"
sbatch --dependency=afterok:$job_ids HPC/compile-friedman-sim.sh