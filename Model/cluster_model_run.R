################################################################################
#
# Cluster Model Run
#
# This is the script used to produce the paper's simulation data on the
# HPC cluster at Freie Universitaet Berlin. Submit via run_model.sh.
#
# Authors: Stav Gelber, Britta Tietjen, Felix May
#
################################################################################

library(raster)
library(data.table)
library(tibble)
library(foreach)

source("src/generate_grid.R")
source("src/generate_agents.R")
source("src/distribute_agents.R")
source("src/environment_variation.R")
source("src/disturbance.R")
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

# Cluster arguments -------------------------------------------------------

args <- commandArgs(trailingOnly = TRUE)
task_id <- args[1]
workers <- as.integer(args[2])
exp_name <- "unique_dispersal_"
exp_num <- "101"  # adjust per experiment

# Parallel setup ----------------------------------------------------------

my.cluster <- parallel::makeCluster(
  workers,
  port = 11000 + as.integer(task_id),
  outfile = "",
  type = "FORK"
)

doParallel::registerDoParallel(cl = my.cluster)

result_par <- foreach(
  i = 1:nrow(var_par),
  .packages = c("data.table", "raster")
) %dopar% {
  GeDo_run(
    mod_par = mod_par,
    var_par = var_par[i, ],
    switch = switch,
    file_name = exp_name,
    task_id = 1,
    sim_id = i
  )
}

parallel::stopCluster(cl = my.cluster)

# Combine and write results -----------------------------------------------

static_par <- rbind(as.data.frame(t(switch)), as.data.frame(t(mod_par)))
write.csv(static_par, file = paste0("Outputs/", exp_name, exp_num, "_static_parameters.csv"))

var_par_write <- rowid_to_column(var_par, "sim_ID")
write.csv(var_par_write, file = paste0("Outputs/", exp_name, exp_num, "_varaying_parameters.csv"), row.names = FALSE)

result_par <- purrr::transpose(result_par)

output_all <- data.table::rbindlist(result_par[["output_all"]])
output_sample <- data.table::rbindlist(result_par[["output_sample"]])
output_dispersal <- data.table::rbindlist(result_par[["output_dispersal"]])

data.table::fwrite(x = output_all, file = paste0("Outputs/", exp_name, exp_num, "_rep_", task_id, "_output_general.csv"), sep = ",")
data.table::fwrite(x = output_sample, file = paste0("Outputs/", exp_name, exp_num, "_rep_", task_id, "_output_sample.csv"), sep = ",")
data.table::fwrite(x = output_dispersal, file = paste0("Outputs/", exp_name, exp_num, "_rep_", task_id, "_output_dispersal.csv"), sep = ",")
