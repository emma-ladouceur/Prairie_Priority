#!/bin/bash

#SBATCH -J alpha_cover
#SBATCH --time=8:00:00
#SBATCH --mem-per-cpu=8G
#SBATCH --cpus-per-task=4

# Output files should ideally go to /work instead of /home
#SBATCH -o /work/%u/%x-%j.out

module load foss/2019b R/3.6.2-2
export OFILE=/work/$USER/Prairie_Priority/$SLURM_JOB_NAME-$SLURM_JOB_ID.Rdata
export LANG=en_US.UTF-8
Rscript --vanilla /home/ladouceu/projects/Prairie_Priority/cluster/alpha_cover.R


