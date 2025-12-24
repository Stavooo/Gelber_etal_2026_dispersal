#!/bin/bash -e

#SBATCH --job-name=dispersal_model
#SBATCH --mail-user=
#SBATCH --mail-type=end
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=32
#SBATCH --mem-per-cpu=700
#SBATCH --time=4:00:00
#SBATCH --qos=standard
#SBATCH --array=1-10

module add R

Rscript "cluster_model_run.R" ${SLURM_ARRAY_TASK_ID} ${SLURM_CPUS_PER_TASK}
