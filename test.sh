#!/bin/bash
#SBATCH --account=def-pjames   # replace this with your own account
#SBATCH --mem-per-cpu=8000M      # memory; default unit is megabytes
#SBATCH --time=00-24           # time (DD-HH)
#SBATCH --ntasks=1
module load r # Adjust version and add the gcc module used for installing packages.
module load gcc
module load udunits
module load gdal
cd ~/project/p1092272/Globus/Genetic_TBI_LCBD
Rscript ForCC.R

