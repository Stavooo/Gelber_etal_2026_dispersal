################################################################################
#
# Run Model Locally
#
# This script demonstrates how to run the model locally.
# For the paper's simulations, the model was run on a cluster using
# cluster_model_run.R with parameters defined in parameters.R.
#
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

# Source model functions --------------------------------------------------

source("Model/src/generate_grid.R")
source("Model/src/generate_agents.R")
source("Model/src/distribute_agents.R")
source("Model/src/environment_variation.R")
source("Model/src/disturbance.R")
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

exp_name <- "test_run"
exp_num <- "1"
task_id <- 1

# Single model run --------------------------------------------------------

result <- GeDo_run(
  mod_par = mod_par,
  var_par = var_par[1, ],
  switch = switch,
  file_name = exp_name,
  task_id = task_id,
  sim_id = 1
)

# Save results
data.table::fwrite(
  x = result[["output_all"]],
  file = paste0("Model/Outputs/", exp_name, "_rep_", task_id, "_output_general.csv"),
  sep = ","
)

data.table::fwrite(
  x = result[["output_sample"]],
  file = paste0("Model/Outputs/", exp_name, "_rep_", task_id, "_output_sample.csv"),
  sep = ","
)

data.table::fwrite(
  x = result[["output_dispersal"]],
  file = paste0("Model/Outputs/", exp_name, "_rep_", task_id, "_output_dispersal.csv"),
  sep = ","
)

# Save parameter settings
static_par <- rbind(as.data.frame(t(switch)), as.data.frame(t(mod_par)))
write.csv(static_par, file = paste0("Model/Outputs/", exp_name, "_", exp_num, "_static_parameters.csv"))

var_par_write <- tibble::rowid_to_column(var_par, "sim_ID")
write.csv(var_par_write, file = paste0("Model/Outputs/", exp_name, "_", exp_num, "_varaying_parameters.csv"), row.names = FALSE)
