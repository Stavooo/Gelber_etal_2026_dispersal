################################################################################
#
# Cluster Model Run Script
#
# This script runs the fragmentation model on a computing cluster using
# parallel processing.
#
# Usage: Rscript cluster_model_run.R <task_id> <workers>
#   task_id: Unique identifier for this run
#   workers: Number of parallel workers to use
#
# Author: Stav Gelber
# Paper: Disturbance and landscape characteristics interactively drive 
#        dispersal strategies in continuous and fragmented metacommunities
# Authors: Stav Gelber, Britta Tietjen, Felix May
#
################################################################################

# Load packages -----------------------------------------------------------

library(raster)
library(data.table)
library(tibble)
library(foreach)
library(doParallel)
library(purrr)
library(vegan)
library(dplyr)

# Source model functions --------------------------------------------------

source("src/generate_grid.R")
source("src/generate_agents.R")
source("src/distribute_agents.R")
source("src/birth.R")
source("src/death.R")
source("src/initialize.R")
source("src/GeDo_run.R")
source("src/cookie_cutting.R")
source("src/immigration.R")
source("src/landscape.R")
source("src/disperse.R")
source("src/initialize_result_files.R")
source("parameters.R")


# Get command line arguments ----------------------------------------------

args <- commandArgs(trailingOnly = TRUE)
task_id <- args[1]
workers <- as.integer(args[2])

# Set experiment name
exp_name <- "cluster_run"
exp_num <- "01" # Change for each new experiment

print(paste0("Running with ", workers, " workers"))
print(paste0("Available cores: ", parallel::detectCores()))


# Setup parallel processing -----------------------------------------------

my_cluster <- parallel::makeCluster(
  workers,
  port = 11000 + as.integer(task_id),
  outfile = "",
  type = "FORK" # Use "FORK" on Linux/Mac, "PSOCK" on Windows
)

# Check cluster definition (optional)
print("Cluster definition:")
print(my_cluster)

# Register parallel backend
doParallel::registerDoParallel(cl = my_cluster)

# Verify registration (optional)
print("Cluster registered:")
print(foreach::getDoParRegistered())

# Check number of workers (optional)
print("Number of workers:")
print(foreach::getDoParWorkers())


# Run model simulations in parallel ---------------------------------------

result_par <- foreach(
  i = 1:nrow(var_par),
  .packages = c("data.table", "raster", "vegan", "dplyr", "tibble")
) %dopar% {
  GeDo_run(
    mod_par = mod_par,
    var_par = var_par[i, ],
    switch = switch,
    file_name = exp_name,
    task_id = task_id,
    sim_id = i
  )
}

# Stop the cluster
parallel::stopCluster(cl = my_cluster)


# Combine and write results -----------------------------------------------

# Save static parameters
static_par <- rbind(as.data.frame(t(switch)), as.data.frame(t(mod_par)))
write.csv(
  static_par,
  file = paste0("Outputs/", exp_name, exp_num, "_static_parameters.csv")
)

# Save varying parameters
var_par_write <- var_par
var_par_write <- tibble::rowid_to_column(var_par_write, "sim_ID")
write.csv(
  var_par_write,
  file = paste0("Outputs/", exp_name, exp_num, "_varying_parameters.csv"),
  row.names = FALSE
)

# Transpose results list
result_list <- purrr::transpose(result_par)

# Combine all results
output_all <- data.table::rbindlist(result_list[["output_all"]])
output_sample <- data.table::rbindlist(result_list[["output_sample"]])
output_dispersal <- data.table::rbindlist(result_list[["output_dispersal"]])

# Write combined results to disk
data.table::fwrite(
  x = output_all,
  file = paste0(
    "Outputs/",
    exp_name, exp_num, "_rep_", task_id, "_output_general.csv"
  ),
  sep = ","
)

data.table::fwrite(
  x = output_sample,
  file = paste0(
    "Outputs/",
    exp_name, exp_num, "_rep_", task_id, "_output_sample.csv"
  ),
  sep = ","
)

data.table::fwrite(
  x = output_dispersal,
  file = paste0(
    "Outputs/",
    exp_name, exp_num, "_rep_", task_id, "_output_dispersal.csv"
  ),
  sep = ","
)

print(paste0("Task ", task_id, " completed successfully!"))
