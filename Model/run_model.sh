#!/bin/bash -e

#SBATCH --job-name=uni_disp
#SBATCH --mail-user=stav.gelber@fu-berlin.de
#SBATCH --mail-type=end
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=16
#SBATCH --mem-per-cpu=1200M
#SBATCH --time=40:00:00
#SBATCH --qos=standard
#SBATCH --array=1-20

module add R
module add GDAL

Rscript "cluster_model_run.R" ${SLURM_ARRAY_TASK_ID} ${SLURM_CPUS_PER_TASK}
