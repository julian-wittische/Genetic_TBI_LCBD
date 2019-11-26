#!/bin/bash
#SBATCH --account=def-pjames   # replace this with your own account
#SBATCH --mem=8000M      # memory; default unit is megabytes
#SBATCH --time=00-24           # time (DD-HH)
#SBATCH --array=1-18
#SBATCH --job-name=jobarray
module load r # Adjust version and add the gcc module used for installing packages.
module load gcc
module load udunits
module load gdal
echo "Starting task $SLURM_ARRAY_TASK_ID"
DIR=$(sed -n "${SLURM_ARRAY_TASK_ID}p" case_list)
cd $DIR
cd output
Rscript ForCC.R


