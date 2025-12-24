################################################################################
#
# Run Model Script
#
# This script shows examples of how to run the fragmentation model:
# 1. Single model run
# 2. Multiple runs in a loop (sequential)
# 3. Multiple runs in parallel
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

source("Model/src/generate_grid.R")
source("Model/src/generate_agents.R")
source("Model/src/distribute_agents.R")
source("Model/src/birth.R")
source("Model/src/death.R")
source("Model/src/initialize.R")
source("Model/src/GeDo_run.R")
source("Model/src/cookie_cutting.R")
source("Model/src/immigration.R")
source("Model/src/landscape.R")
source("Model/src/disperse.R")
source("Model/src/initialize_result_files.R")
source("Model/parameters.R")


# Setup -------------------------------------------------------------------

# Set experiment name
exp_name <- "test_run"

# Set task ID for file naming
task_id <- 1


# Example 1: Single model run ---------------------------------------------

# Run one model repetition for the first parameter set
result <- GeDo_run(
  mod_par = mod_par,
  var_par = var_par[1, ],
  switch = switch,
  file_name = exp_name,
  task_id = task_id,
  sim_id = 1
)

# Save the results
data.table::fwrite(
  x = result[["output_all"]],
  file = paste0(
    "Model/Outputs/",
    exp_name, "_rep_", task_id, "_output_general.csv"
  ),
  sep = ","
)

data.table::fwrite(
  x = result[["output_sample"]],
  file = paste0(
    "Model/Outputs/",
    exp_name, "_rep_", task_id, "_output_sample.csv"
  ),
  sep = ","
)

data.table::fwrite(
  x = result[["output_dispersal"]],
  file = paste0(
    "Model/Outputs/",
    exp_name, "_rep_", task_id, "_output_dispersal.csv"
  ),
  sep = ","
)


# Example 2: Run in a loop (sequential) ----------------------------------

# Number of simulations to run
n_sims <- min(5, nrow(var_par))

result_seq <- foreach(
  i = 1:n_sims
) %do% {
  GeDo_run(
    mod_par = mod_par,
    var_par = var_par[i, ],
    switch = switch,
    file_name = exp_name,
    task_id = task_id,
    sim_id = i
  )
}

# Transpose the list so all output_all and all output_sample are together
result_list <- purrr::transpose(result_seq)

# Combine all results
output_all <- data.table::rbindlist(result_list[["output_all"]])
output_sample <- data.table::rbindlist(result_list[["output_sample"]])
output_dispersal <- data.table::rbindlist(result_list[["output_dispersal"]])

# Write combined results to disk
data.table::fwrite(
  x = output_all,
  file = paste0(
    "Model/Outputs/",
    exp_name, "_sequential_output_general.csv"
  ),
  sep = ","
)

data.table::fwrite(
  x = output_sample,
  file = paste0(
    "Model/Outputs/",
    exp_name, "_sequential_output_sample.csv"
  ),
  sep = ","
)

data.table::fwrite(
  x = output_dispersal,
  file = paste0(
    "Model/Outputs/",
    exp_name, "_sequential_output_dispersal.csv"
  ),
  sep = ","
)


# Example 3: Run in parallel ----------------------------------------------

# Set number of cores to use (1 less than available)
workers <- parallel::detectCores() - 1

# Create parallel cluster
my_cluster <- parallel::makeCluster(
  workers,
  type = "PSOCK" # PSOCK works on both Windows and UNIX
)

# Register the parallel backend
doParallel::registerDoParallel(cl = my_cluster)

# Run simulations in parallel
result_par <- foreach(
  i = 1:n_sims,
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

# Transpose the list
result_list <- purrr::transpose(result_par)

# Combine all results
output_all <- data.table::rbindlist(result_list[["output_all"]])
output_sample <- data.table::rbindlist(result_list[["output_sample"]])
output_dispersal <- data.table::rbindlist(result_list[["output_dispersal"]])

# Write to disk
data.table::fwrite(
  x = output_all,
  file = paste0(
    "Model/Outputs/",
    exp_name, "_parallel_output_general.csv"
  ),
  sep = ","
)

data.table::fwrite(
  x = output_sample,
  file = paste0(
    "Model/Outputs/",
    exp_name, "_parallel_output_sample.csv"
  ),
  sep = ","
)

data.table::fwrite(
  x = output_dispersal,
  file = paste0(
    "Model/Outputs/",
    exp_name, "_parallel_output_dispersal.csv"
  ),
  sep = ","
)

# Save parameter settings
static_par <- rbind(as.data.frame(t(switch)), as.data.frame(t(mod_par)))
write.csv(
  static_par,
  file = paste0("Model/Outputs/", exp_name, "_static_parameters.csv")
)

var_par_write <- var_par
var_par_write <- tibble::rowid_to_column(var_par_write, "sim_ID")
write.csv(
  var_par_write,
  file = paste0("Model/Outputs/", exp_name, "_varying_parameters.csv"),
  row.names = FALSE
)

print("Model run completed successfully!")
